# load packages ----------------------------------------------------------------
source("vpi_plot.R")
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)


# prepare the data for the app -------------------------------------------------
VPI_data <- transform_data()                    # download data and reshape it 
table_data_download <- prepare_table(VPI_data)  # prepare table for download
products_list <- VPI_data %>%                   # get vector of product names
  distinct(Produkt) %>%
  pull(Produkt)

start_month <- "2020-01-01"     # define start date, adjust for new base year
end_month <- today()            # set the start day to today's date


# user interface ---------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("cosmo"),               # load the CSS theme
  titlePanel("Inflationsdashboard"),         # set title of web page
  navbarPage(                                # set up the 3 tabs
    "",                                      # empty name for tabs
    tabPanel(                                # tab with the introduction
      "Einleitung",
      includeMarkdown("./Einleitung.Rmd")
    ),
    tabPanel(                                # tab with the plotting tool
      "Datenvisualisierung",                 # name of the tab
      sidebarLayout(                         # side bar with the date slider
        sidebarPanel(
          dateRangeInput("date_input",
            label = "Zeitraum",
            start = start_month,
            end = end_month,
            min = "1991-01-01",
            max = end_month,
            language = "de"
          ),
          selectInput("products",            # select products for display 
            label = "Produkte",
            choices = NULL,
            multiple = TRUE,
            selectize = TRUE
          )
        ),
        mainPanel(                           # main panel with the plot and
          textOutput("Plot_message"),        # description
          plotlyOutput("VPI_TS"),
          includeMarkdown("./Beschreibung_Zeitreihe.Rmd")
        )
      )
    ),
    tabPanel(                                # tab the with table for downloading
      "Tabelle",
      dataTableOutput("table"),
      includeMarkdown("./Beschreibung_Tabelle.Rmd")
    )
  )
)


# server function --------------------------------------------------------------
server <- function(input, output, session) {
  
  # get the products from the select tool for the plots
  updateSelectizeInput(session, 
                       "products",
                       choices = products_list,
                       server = TRUE)

  # render the plot
  output$VPI_TS <- renderPlotly({
    # validate that at least one product is selected by the user
    validate(
      need(length(input$products) > 0, "Bitte wähle links ein oder mehrere Produkte aus.")
    )
    date_start <- input$date_input[[1]]     # define the start and end date
    date_end <- input$date_input[[2]]
    plot_price_index(VPI_data,              # call the plot function
      product_names = input$products,
      start_month = date_start,
      end_month = date_end
    )
  })

  # render the data table and provide buttons for downloading the data
  output$table <- renderDataTable(datatable(table_data_download,
    filter = "top",
    extensions = "Buttons",
    options = list(
      pageLength = 10,
      language = list(url = "https:cdn.datatables.net/plug-ins/1.10.11/i18n/German.json"),
      autoWidth = TRUE,
      rownames = FALSE,
      lengthMenu = list(c(10, 50, 100, -1), c("10", "50", "100", "all")),
      dom = "Blfrtip",
      buttons = list(
        extend = "csv",
        filename = "data.csv",
        title = NULL,
        text = "Download CSV"
      ),
      caption = "Tabelle mit Indexwerten",
      selection = "single",
      colnames = c(
        "Produktcode", "Monat", "Preisindex", "Produktname",
        "Änderung zum Vormonat [%]",
        "Änderung zum Vorjahresmonat [%]"
      ),
      class = "display"
    )
  )
  )
}


# Run the application-----------------------------------------------------------
shinyApp(ui = ui, server = server)
