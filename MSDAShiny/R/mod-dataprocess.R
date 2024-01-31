# dataprocess handles viewing of the preprocessed data for checking by the user
# as well as starts rest of the data processing (protein summarization,
# group comparisons)
# once initial processing module works add a way to select different
# groups for comparisons

dataprocess_ui <- function(id) {
  
  ns <- NS(id)

  fluidPage(
    actionButton(ns("groupcomparisons"), "Perform group comparisons"),
    # DTOutput(ns("table")),
  )
}

# dataprocess_server needs myData from dataupload_server to further process the data

dataprocess_server <- function(id, dataupload_data) {

  moduleServer(id, function(input, output, session) {
    
    values <- reactiveValues(groupcomp_data = NULL)
    
    observe({
      values$groupcomp_data <- data_groupcomparisons(dataupload_data$preprocessed_data)
    }) %>% bindEvent(input$groupcomparisons)

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