# inflationsdashboard-de
*R Shiny App for tracking the inflation in Germany*

This is a simple dashboard for tracking the inflation in Germany by using official data from the Federal Office of Statistics. The source code is licenced under the MIT licence, while the data has the license Datenlizenz-Deutschland (http://www.govdata.de/dl-de/by-2-0)

To run this shiny web app, download the repository and run it with R Shiny and the necessary packages. You have to adjust the source code for the login credentials for the Genesis Online database (https://www-genesis.destatis.de/genesis/online?operation=sprachwechsel&language=en) in the file
*vpi_plot_public.R* by inserting your login credentials and rename it to *vpi_plot.R*.

The dashboard is also available under [https://inflationsdashboard-de.shinyapps.io/inflation/], which is hosted by shinyapps.io. However only 25 active hours each month are possible.
