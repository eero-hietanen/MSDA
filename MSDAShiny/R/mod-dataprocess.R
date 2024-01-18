# dataprocess handles viewing of the preprocessed data for checking by the user
# as well as starts rest of the data processing (protein summarization,
# group comparisons)
# once initial processing module works add a way to select different
# groups for comparisons

dataprocess_ui <- function(id) {
  
  ns <- NS(id)

  fluidPage(
    dataTableOutput(ns("preprocessed_data"))
  )
}

dataprocess_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    output$preprocessed_data <- DT::renderDataTable({
      
      dataOut
      
      # https://stackoverflow.com/questions/73071711/r-shiny-how-to-pass-reactive-filtered-dataframe-between-modules
      
      
    })
    
  })
  
}

processTest <- function() {

  ui <- fluidPage(dataprocess_ui("x"))
  server <- function(input, output, session){
    dataprocess_server("x")
  }

  shinyApp(ui, server)
}