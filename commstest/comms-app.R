#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyjs)
library(shinybusy)
library(shinyFeedback)
library(MSstats)
library(MSstatsTMT)
library(MSstatsConvert)
library(data.table)
library(UniProt.ws)
library(DT)
library(bslib)
library(tidyverse)
library(EnhancedVolcano)
library(ggplot2)
library(gridlayout)


useShinyjs()
useShinyFeedback() 

# Define UI for application that draws a histogram
ui <- fluidPage(

  useShinyjs(),
  theme = bs_theme(bootswatch = "flatly"),
  useShinyFeedback(),

    navbarPage("App",
               tabPanel("Data",
                          mainPanel(
                            data_ui("data")
                          )
               ),
               tabPanel("Plot",
                        mainPanel(
                          plot_ui("plot")
                        )
               ),
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data_vals <- data_server("data")
  plot_server("plot", data_vals)

}

# Run the application 
shinyApp(ui = ui, server = server)
