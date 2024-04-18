# main app calls modules and wraps everything together
# planned app steps are: 1) data upload, 2) data processing, 3) plotting

# read about renderUI() and its use for dynamic UIs
# TODO: add a mode where the user enters necessary options to do everything up to the final Uniprot table generation
# TODO: Plotting: add a functionality to the plotting module that lets you downloda the up and/or down regulated genes.
#       this should likely be in the format of the UniProt table, just limited to the data selection (e.g. up or down reg.)-
#       Look up examples of linked data selection, e.g., select points from a plot <-> select rows from table.
# TODO: Data processing: Since there are so many different UniProt fields, add text field input for which UniProt fields to return.
#       The user should be directed to https://www.uniprot.org/help/return_fields, or make a list of all the field codes in the app.
#       The user will then enter all the fields they want to retrieve (whitespace delimited). Parse the input and add each element to the
#       query as a column to retrieve.
# TODO: Fix busy spinner background.
# TODO: Implement accordions to organize the side panel settings into logical groups
# TODO: Adjust download button widths
# TODO: Look into crosstalk library to link plots with DTs

library(shiny)
library(shinyjs)
library(shinybusy)
library(shinyFeedback)
library(shinythemes)
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
library(ggrepel)
library(plotly)
library(gridlayout)
library(thematic)
library(crosstalk)
library(colourpicker)

options(shiny.maxRequestSize = 40 * 1024^2)
# options(shiny.error = NULL)
thematic_shiny()
theme_set(theme_minimal())

# tagList(
#   tags$head(
#     # CSS style edits with bslib, and directed to specific elements (e.g. datatables).
#     tags$style(".datatables td {padding-top: 1px; padding-bottom: 1px; font-size: 70%;}"),
#   )
# )

ui <- page_navbar(

  # theme = bs_theme(bootswatch = "flatly"),
  
  # Loading things in the header due to bslib complaining otherwise and loading Shinyjs and feedback outside the UI doesn't work
  header = tagList(
    useShinyjs(),
    useShinyFeedback()
    ),
  
  # update params. for bs_theme_update were obtained through bs_themer().
  # also pipes to bs_add_rules() using |>, which is then used to update the CSS for, e.g., data tables
  theme = bs_theme_update(bs_theme(bootswatch = "flatly"), bg = "rgb(249, 249, 249)", primary = "#34516D",
                  success = "#8278AC", font_scale = NULL, `enable-rounded` = FALSE,
                  fg = "#000") |> bs_add_rules(
                    list(
                      ".datatables td {padding-top: 2px; padding-bottom: 2px; font-size: 80%}"
                        )
                    ),

  #Check shinyjs and hidden ('shinyjs::hidden') as a way to initialize hidden UI

  title = "MSDA Shiny",
  
  nav_panel("Data upload", dataupload_ui("upload")),
  nav_panel("Data processing", dataprocess_ui("process")),
  nav_panel("Plotting", plotting_ui("plotting")),
  
  padding = "3px",

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
  
  # bs_themer()
  upload_values <- dataupload_server("upload")
  
  dataprocess_values <- dataprocess_server("process", upload_values)
  # 
  # # Call the server function of the plotting module
  plotting_values <- plotting_server("plotting", dataprocess_values)
  
  # output$preprocessed_table <- renderDT(upload_values$preprocessed_data) # output in mod-dataupload
  
  #check why these 3 don't show up if they're disabled in the main app (compare to dataupload module, which works)
  # output$groupcomp_table <- renderDT(dataprocess_values$groupcomp_data) # output in mod-dataprocess
  # output$uniprot_table <- renderDT(dataprocess_values$uniprot_data) # output in mod-dataprocess
  # output$uniprot_species <- renderDT(dataprocess_values$uniprot_species) # output in mod-dataprocess
  # 
  # output$plot_output <- renderPlotly(plotting_values$p1) # output in mod-plotting
  # output$plot_output2 <- renderPlot(plotting_values$p2) # output in mod-plotting
 
}


shinyApp(ui, server)
