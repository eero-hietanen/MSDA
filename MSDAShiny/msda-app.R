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

options(shiny.maxRequestSize = 40 * 1024^2)

ui <- fluidPage(
  
  useShinyjs(),
  # add_busy_spinner(spin = "orbit", color = "#f0bc13"),
  
  fluidRow(
    
    dataupload_ui("upload"),
    dataprocess_ui("process"),
    # plotting_ui("plot"),
  )
) 

server <- function(input, output, session) {
  
  dataupload_server("upload")
  dataprocess_server("process")
  # plotting_server("plot")
  
}

shinyApp(ui, server)
