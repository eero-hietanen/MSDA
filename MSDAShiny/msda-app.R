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
  #tags on app launch which can later be made visible with show(). These might have
  #to be placed in the module UI part in combination with shinyjs 'onclick' functionality.
  
  #>>> navlistpanel layout
  titlePanel("MSDA Shiny"),
  
  # fluidrow with a navlist panel for inputs for each section
  # if data upload and processing want as separate nav pages, then the buttons
  # should be moved to the respective modules (dataprocess module etc.)
  fluidRow(
  navlistPanel(
    tabPanel("Data upload", dataupload_ui("upload")),
    tabPanel("Data processing", dataprocess_ui("process")), #data processing related inputs (necessary?)
    tabPanel("Plotting", plotting_ui("plotting")), #plotting related inputs
  )
  ),
  
  # fluidrow with a mainpanel that acts as an output area for tables/plots produced by the modules
  # visibility has to be controlled based on user input (i.e. selection or what nav list page is open)
  fluidRow(
    mainPanel(
      tags$hr(),
        tabsetPanel(id = "output_tables", type = "pills",
                    tabPanel(id = "preprocessed_table", "Preprocessed data", DTOutput("preprocessed_table")),
                    tabPanel(id = "groupcomp_table", "Group comp data", DTOutput("groupcomp_table")),
                    tabPanel(id = "plot_output", "Volcano Plot", renderPlot("plot_output")),
        )
    )
  )
  #<<< navlistpanel layout
  
  #>>> sidebar layout
  # sidebarLayout(
  # 
  #   sidebarPanel(
  #     dataupload_ui("upload")
  #   ),
  # 
  #   mainPanel(
  #     tabsetPanel(id = "output_tables", type = "tabs",
  #                 tabPanel(id = "preproc_tbl", "Preprocessed data", DTOutput("preprocessed_table")),
  #                 tabPanel(id = "groupcomp_tbl", "Group comparison data", DTOutput("groupcomp_table")),
  #     )
  #   )
  # )
  #<<< sidebar layout
  
  #>>> row layout
  # fluidRow(
  # 
  #   column(5,
  #          dataupload_ui("upload"),
  #          # dataprocess_ui("process")
  #          ),
  #   # column(7, plotting_ui("plot")),
  #   # dataupload_ui("upload"),
  #   # dataprocess_ui("process"),
  #   # plotting_ui("plot"),
  # ),
  # 
  # mainPanel(
  #   tabsetPanel(id = "output_tables", type = "tabs",
  #     tabPanel(id = "preproc_tbl", "Preprocessed data", DTOutput("preprocessed_table")),
  #     tabPanel(id = "groupcomp_tbl", "Group comparison data", DTOutput("groupcomp_table")),
  #   )
  # )
  #<<< row layout

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
  
  # hide("output_tables")
  
  upload_values <- dataupload_server("upload")
  # upload_values() being called below the way it is results in the table output depending on the code order in data upload module
  # i.e., if reactive(preprocessed_data()) is called last in the module, the table shows the preprocessed data table
  # look into update funcs for output
  
  # call dataprocess module with dataupload module input
  # dataprocess returns the group comparison data
  
  dataprocess_values <- dataprocess_server("process", upload_values)
  # dataprocess_server("process", reactive(values))
  plotting_values <- plotting_server("plotting", dataprocess_values) # plot function in utils.R works, but module communication has a problem
  
  # change these to be dynamically shown
  output$preprocessed_table <- renderDT(upload_values$preprocessed_data)
  output$groupcomp_table <- renderDT(dataprocess_values$groupcomp_data)
  output$plot_output <- renderImage(plotting_values) #this needs to call plot(<ggplotobject>) here, and also be reactive
  
  # below the 'input$preprocess' and 'input$groupcomparisons' inputs are accessed through the dataupload_ui call
  # observing the event works, but changing the tab visibility doesn't quite work; check insertTab()
  # observeEvent(input$preprocess, {
  # 
  #   updateTabsetPanel(session, "output_tables", selected = "preprocessed_table")
  #              
  # })
  # 
  # observeEvent(input$groupcomparisons, {
  #   
  #   updateTabsetPanel(session, "output_tables", selected = "groupcomp_table")
  #             
  # })

  
}

shinyApp(ui, server)
