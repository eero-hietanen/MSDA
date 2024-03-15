# dataupload handles uploading the initial evidence and annotation files
# as well as preprocessing the data to msstatstmt format

dataupload_ui <- function(id) {
  
  useShinyjs()
  ns <- NS(id)

  tagList(
    fileInput(ns("evidence"), "Evidence"),
    fileInput(ns("annotation"), "Annotation"),
    actionButton(ns("preprocess"), "Preprocess files"),
  )

}

dataupload_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {

    values <- reactiveValues(preprocessed_data = NULL)
    
    observe({
      values$preprocessed_data <- data_preprocessing(input$evidence, input$annotation)
    }) %>% bindEvent(input$preprocess)

    output$preprocessed_table <- renderDT(values$preprocessed_data)
    
    # reactive({
    #   list(preprocessed_data = values$preprocessed_data)
    # })
    
    values
    
  })
  
}

uploadTest <- function() {
  
  ui <- fluidPage(dataupload_ui("x"))
  server <- function(input, output, session){
    dataupload_server("x")
  }
  
  shinyApp(ui, server)
}
