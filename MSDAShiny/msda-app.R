# main app calls modules and wraps everything together
# planned app steps are: 1) data upload, 2) data processing, 3) plotting

# read about renderUI() and its use for dynamic UIs

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
library(plotly)

options(shiny.maxRequestSize = 40 * 1024^2)
# options(shiny.error = NULL)

ui <- fluidPage(
  
  useShinyjs(),
  useShinyFeedback(),
  theme = bs_theme(bootswatch = "flatly"),
  
  #Check shinyjs and hidden ('shinyjs::hidden') as a way to initialize hidden UI

  # titlePanel("MSDA Shiny"),
  
  navbarPage("MSDA Shiny",
             tabPanel("Data upload",
                      sidebarLayout(
                        sidebarPanel(dataupload_ui("upload"), width = 2),
                        mainPanel(DTOutput("preprocessed_table"))
                      )
             ),
             tabPanel("Data processing",
                      sidebarLayout(
                        sidebarPanel(dataprocess_ui("process"), width = 2),
                        mainPanel(
                          DTOutput("groupcomp_table"),
                          DTOutput("uniprot_table"),
                          DTOutput("uniprot_species"),
                          )
                      ),
                      
             ),
             tabPanel("Plotting",
                      sidebarLayout(
                        sidebarPanel(plotting_ui("plotting"), width = 2),
                        mainPanel(
                          plotlyOutput("plot_output"),
                          plotOutput("plot_output2"),
                        )
                      ),
                      
             ),
  )
  
                        
                        
  # fluidRow(
  # navlistPanel(
  #   tabPanel("Data upload", dataupload_ui("upload")),
  #   tabPanel("Data processing", dataprocess_ui("process")),
  #   tabPanel("Plotting", plotting_ui("plotting")),
  # )
  # ),
  # 
  # fluidRow(
  #   mainPanel(
  #     tags$hr(),
  #       tabsetPanel(id = "output_tables", type = "pills",
  #                   tabPanel(id = "preprocessed_table", "Preprocessed data", DTOutput("preprocessed_table")),
  #                   tabPanel(id = "groupcomp_table", "Group comp data", DTOutput("groupcomp_table")),
  #                   tabPanel(id = "uniprot_table", "Uniprot data", DTOutput("uniprot_table")),
  #                   tabPanel(id = "plot_output", "Volcano Plot", plotOutput("plot_output")),
  #                   tabPanel(id = "plot_output2", "Enhanced Volc. Plot", plotOutput("plot_output2")),
  #                   tabPanel(id = "uniprot_species", "Uniprot species", DTOutput("uniprot_species")),
  #       )
  #   )
  # )
)

server <- function(input, output, session) {
  
  upload_values <- dataupload_server("upload")
  
  dataprocess_values <- dataprocess_server("process", upload_values)
  
  # Call the server function of the plotting module
  plotting_values <- plotting_server("plotting", dataprocess_values)
  
  output$preprocessed_table <- renderDT(upload_values$preprocessed_data)
  output$groupcomp_table <- renderDT(dataprocess_values$groupcomp_data)
  output$uniprot_table <- renderDT(dataprocess_values$uniprot_data)
  output$uniprot_species <- renderDT(dataprocess_values$uniprot_species)
  output$plot_output <- renderPlotly(plotting_values$p1)
  output$plot_output2 <- renderPlot(plotting_values$p2)
 
}


shinyApp(ui, server)
