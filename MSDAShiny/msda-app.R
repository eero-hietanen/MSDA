# main app calls modules and wraps everything together
# planned app steps are: 1) data upload, 2) data processing, 3) plotting

# read about renderUI() and its use for dynamic UIs


library(shiny)
library(shinyjs)
library(shinybusy)
library(MSstats)
library(MSstatsTMT)
library(MSstatsConvert)
library(data.table)
library(UniProt.ws)
library(DT)
library(bslib)
library(tidyverse)

options(shiny.maxRequestSize = 40 * 1024^2)
# options(shiny.error = NULL)

ui <- fluidPage(
  
  useShinyjs(),
  theme = bs_theme(bootswatch = "flatly"),
  
  #Check shinyjs and hidden ('shinyjs::hidden') as a way to initialize hidden UI

  titlePanel("MSDA Shiny"),
  
  fluidRow(
  navlistPanel(
    tabPanel("Data upload", dataupload_ui("upload")),
    tabPanel("Data processing", dataprocess_ui("process")),
    tabPanel("Plotting", plotting_ui("plotting")),
  )
  ),

  fluidRow(
    mainPanel(
      tags$hr(),
        tabsetPanel(id = "output_tables", type = "pills",
                    tabPanel(id = "preprocessed_table", "Preprocessed data", DTOutput("preprocessed_table")),
                    tabPanel(id = "groupcomp_table", "Group comp data", DTOutput("groupcomp_table")),
                    tabPanel(id = "plot_output", "Volcano Plot", plotOutput("plot_output")),
        )
    )
  )

)

server <- function(input, output, session) {

  upload_values <- dataupload_server("upload")

  dataprocess_values <- dataprocess_server("process", upload_values)
  plotting_values <- plotting_server("plotting", dataprocess_values) # plot function in utils.R works, but module communication has a problem
  
  output$preprocessed_table <- renderDT(upload_values$preprocessed_data)
  output$groupcomp_table <- renderDT(dataprocess_values$groupcomp_data)
  output$plot_output <- plotting_values

}

shinyApp(ui, server)
