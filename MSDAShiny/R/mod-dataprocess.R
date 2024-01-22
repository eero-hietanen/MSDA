# dataprocess handles viewing of the preprocessed data for checking by the user
# as well as starts rest of the data processing (protein summarization,
# group comparisons)
# once initial processing module works add a way to select different
# groups for comparisons

dataprocess_ui <- function(id) {
  
  ns <- NS(id)

  fluidPage(
    textOutput(ns("preprocessed_data"))
  )
}

# dataprocess_server needs myData from dataupload_server to further process the data

dataprocess_server <- function(id, myData) {

  moduleServer(id, function(input, output, session) {

    req(myData)
    
    output$preprocessed_data <- renderPrint(myData)
    
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