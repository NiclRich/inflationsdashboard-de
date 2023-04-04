# load packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(wiesbaden)
library(DT)


# define functions -------------------------------------------------------------
#' plot the price data
#' 
#' @details plot the price data as line data for the selected products
#' with *ggplot2*. 
#' 
#' @param data data.frame with the price data.
#' @param product_names list of products to plot.
#' @param start_month First date for the time axis.
#' @param end_motn Last date for the time axis.
plot_price_index <- function(data, product_names, start_month, end_month) {
  data %>%
    filter(Produkt %in% product_names & month >= start_month & month <= end_month) %>% # filter the data
    ggplot(aes(x = month, y = index_value, color = Produkt)) +
    geom_line() +
    geom_hline(linetype = "dotted", yintercept = 100, color = "grey50") + # base line
    theme_light() +
    xlab("Monat") +                # axis titles
    ylab("Preisniveau") +
    scale_x_date(date_breaks = "4 months",
                 minor_breaks = seq(start_month, end_month, "1 month"),
                 date_labels = "%m-%y") +
    ggtitle("Preisentwicklung der Produkte")
}

#' load price data from genesis online
#'
#' load the price price data from genesis online (the database from the
#' Federal Office of Statistics in Germany) with the package *Wiesbaden*. It loads
#' the data for the COICOP at level 10, 5 and 2.
#' 
#' It can place a copy of the data on the local storage. The data is then stored
#' in *genesis_data.Rds*.
#' 
#' @param local_storage Logical, indicates whether the data should be stored as
#' the file *genesis_data.Rds* locally.
#' 
#' @return list of data frames containing the index values
load_data_genesis <- function(local_storage = FALSE) {
  # login data for the database
  genesis_login <- c(db = "de", user = "XXXXXXXXXXX", password = "XXXXXXXXXXXXX")
  
  # retrieve the data for the COICOP goods
  #  COICOP = Classification Of Individual COnsumption by Purpose
  COICOP10_price_index <- retrieve_data(tablename = "61111BM007", genesis = genesis_login) 
  COICOP5_price_index <- retrieve_data(tablename = "61111BM005", genesis = genesis_login)
  COICOP2_price_index <- retrieve_data(tablename = "61111BM002", genesis = genesis_login)
  
  # rename columns
  COICOP10_price_index <- rename(COICOP10_price_index, Code = CC13Z1)
  COICOP5_price_index <- rename(COICOP5_price_index, Code = CC13A5)
  COICOP2_price_index <- rename(COICOP2_price_index, Code = CC13A2)
  
  # make a list of price indices and return them
  price_indices <- list(
    "COICOP10_price_index" = COICOP10_price_index,
    "COICOP5_price_index" = COICOP5_price_index,
    "COICOP2_price_index" = COICOP2_price_index
  )
  
  if (local_storage) saveRDS(price_indices, file = "./genesis_data.Rds")
  return(price_indices)
}

#' transform the data in a relational format for further processing
#' 
#' @details This functions loads the codes with the COICOP codes and
#' their names (*Codes.csv* and *VPI_data.csv*) which are downloaded from
#' the website of the German Federal Office for Statistics and join the
#' data from Genesis Online. 
#' 
#' @param local_storage = FALSE, if TRUE, then it loads it locally, otherwise
#' it is downloaded from genesis online.
transform_data <- function(local_storage = FALSE) {
  
  # load data from genesis online
  price_data <- if(local_storage) readRDS("./genesis_data.Rds") else load_data_genesis() 
  
  # load COICOP Codes
  Codes <- read_delim("Codes.csv",
    delim = ";", escape_double = FALSE, trim_ws = TRUE
  )
  product_codes <- read_delim("VPI_data.csv",
    delim = ";", escape_double = FALSE, col_types = cols(
      Jahr = col_skip(),
      Monat = col_skip(), Monatname = col_skip(),
      VPI = col_skip()
    ), trim_ws = TRUE
  )
  
  # rename columns
  Codes <- rename(Codes, Code = COICOP_Code, Produkt = Produktname)
  product_codes <- rename(product_codes, Code = COICOP10)
  
  # make a join for COICOP Codes at level 2, 5 and 10
  Codes <- full_join(Codes, product_codes, by = c("Code", "Produkt"))
  Codes <- distinct(Codes, .keep_all = TRUE)

  # get the price data at the different levels of the COICOP scheme
  COICOP10_price_index <- price_data[["COICOP10_price_index"]]
  COICOP5_price_index <- price_data[["COICOP5_price_index"]]
  COICOP2_price_index <- price_data[["COICOP2_price_index"]]

  # bind the price indices to a single tibble
  price_indices <- bind_rows(COICOP10_price_index, COICOP5_price_index, COICOP2_price_index)

  # transform data to a relational data format with keys as time reference and
  # reference to the product
  price_indices <- rename(price_indices, index_value = PREIS1_val) %>%
    select(Code, MONAT, JAHR, index_value) %>%
    mutate(
      MONAT = map(MONAT, \(x) gsub("MONAT", "", x)),
      JAHR = map(JAHR, as.character),
      TAG = "01"
    ) %>%
    unite(month, JAHR, MONAT, TAG, sep = "")
  price_indices$month <- ymd(price_indices$month) # convert to date
  
  # join index values with the names
  price_indices <- full_join(price_indices, Codes)
  
  # remove all missing values
  price_indices <- filter(price_indices, !(is.na(month) | is.na(index_value)))

  return(price_indices)
}

#' Prepare data to display in the tab "Tabelle"
#' 
#' @details Compute additional columns (the change to the previous month and
#' change to the same month in the last year) and select suitable columns.
#' 
#' @param price_indices A tibble from the function transform_data 
prepare_table <- function(price_indices) {
  complete_table_data <- price_indices %>%
    ungroup() %>%
    group_by(Code) %>%    # group by product code
    arrange(month, .by_group = TRUE) %>%    # sort by month within each group
    mutate(
      # compute the change to the previous month
      Change_prev_month = (index_value - lag(index_value)) / lag(index_value) * 100,
      # compute the change w.r.t. last year's same month
      Change_smly = (index_value - lag(index_value, 12)) / lag(index_value, 12) * 100
    ) %>% # select suitable columns
    select(Code, month, index_value, Produkt, Change_prev_month, Change_smly) %>%
    mutate(across(c("Change_prev_month", "Change_smly"), round, 1)) %>%
    rename(
      "Aenderung_Vormonat" = Change_prev_month,
      "Aenderung_Vorjahresmonat" = Change_smly,
      "Produktcode" = Code,
      "Produkte" = Produkt,
      "Monat" = month
    ) %>%
    arrange(desc(Monat))

  return(complete_table_data)
}
