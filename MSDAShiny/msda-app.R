# main app calls modules and wraps everything together
# planned app steps are: 1) data upload, 2) data processing, 3) plotting

library(shiny)
library(dplyr)
library(stringr)
library(MSstats)
library(MSstatsTMT)
library(MSstatsConvert)
library(data.table)
library(UniProt.ws)
library(DT)
library(shinyjs)
library(shinybusy)
library(bslib)

options(shiny.maxRequestSize = 40 * 1024^2)
options(shiny.error = NULL)

ui <- fluidPage(
  
  useShinyjs(),
  theme = bs_theme(bootswatch = "flatly"),
  # add_busy_spinner(spin = "orbit", color = "#f0bc13"),
  
  fluidRow(
    
    dataupload_ui("upload"),
    dataprocess_ui("process"),
    # plotting_ui("plot"),
  ),

) 

server <- function(input, output, session) {
  
  # data from dataupload_server should be stored and passed onto dataprocess
  # from SO: set global reactive values that are passed onto each module, update
  # with triggers; update reactive values in each module with observeEvent when
  # triggers are activated
  
  # test the data table output directly from the upload server first without
  # passing the df between modules
  
  # https://stackoverflow.com/questions/74723440/passing-reactives-between-shiny-modules-to-get-dynamic-updates
  # https://stackoverflow.com/questions/68584478/how-to-update-shiny-module-with-reactive-dataframe-from-another-module/68594560#68594560
  # https://stackoverflow.com/questions/69340125/cant-communicate-data-between-shiny-modules
  # https://stackoverflow.com/questions/76140172/modularized-shiny-app-how-to-download-dataset-passed-between-modules
  
  values <- dataupload_server("upload")
  dataprocess_server("process", values)
  # plotting_server("plot")

}

shinyApp(ui, server)
