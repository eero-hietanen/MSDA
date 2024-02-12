# dataprocess handles viewing of the preprocessed data for checking by the user
# as well as starts rest of the data processing (protein summarization,
# group comparisons)
# once initial processing module works add a way to select different
# groups for comparisons

# TODO:
#  - Add options for using a reference channel
#  - Add a comparison selection function, i.e. if there are more groups than just
#    standard 'control' and 'sample'
#  - Add an export function for the UniProt data table
#  - QC plotting options?
#  - Adjustable cutoff values for VolcanoPlot
#  - VolcanoPlot image export
#  - Check https://laustep.github.io/stlahblog/posts/DTcallbacks.html for useful data table callbacks

dataprocess_ui <- function(id) {
  
  ns <- NS(id)

  fluidPage(
    textInput(ns("taxID"), "UniProt taxa ID"),
    actionButton(ns("groupcomparisons"), "Perform group comparisons"),
    actionButton(ns("uniprottable"), "Fetch UniProt data"),
    # DTOutput(ns("table")),
  )
}

# dataprocess_server needs myData from dataupload_server to further process the data

dataprocess_server <- function(id, dataupload_data) {

  moduleServer(id, function(input, output, session) {
    
    values <- reactiveValues(groupcomp_data = NULL, uniprot_data = NULL)
    
    
    observe({
      values$groupcomp_data <- data_groupcomparisons(dataupload_data$preprocessed_data)
    }) %>% bindEvent(input$groupcomparisons)
    
    observe({
      # call util func to fetch uniprot data and construct table; returns the table
      req(values$groupcomp_data)
      taxa <- input$taxID
      validate(
        need(input$taxID != "", "Please enter text.")
      )
      values$uniprot_data <- uniprot_fetch(values$groupcomp_data, taxa)
    }) %>% bindEvent(input$uniprottable)
    
    output$groupcomp_table <- renderDT(values$groupcomp_data)
    
    values
    
  })
  
}

processTest <- function() {

  myData <- iris
  ui <- fluidPage(dataprocess_ui("x"))
  server <- function(input, output, session){
    dataprocess_server("x", reactive({myData}))
  }

  shinyApp(ui, server)
}