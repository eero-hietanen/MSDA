# dataprocess handles viewing of the preprocessed data for checking by the user
# as well as starts rest of the data processing (protein summarization,
# group comparisons)
# once initial processing module works add a way to select different
# groups for comparisons

dataprocess_ui <- function(id) {
  
  ns <- NS(id)

  fluidPage(
    actionButton(ns("process"), "Protein summarization"),
    DTOutput(ns("table")),
  )
}

# dataprocess_server needs myData from dataupload_server to further process the data

dataprocess_server <- function(id, input_data) {

  moduleServer(id, function(input, output, session) {

    # ppData <- eventReactive(input$process, {
    #   
    #   show_modal_spinner(spin = "orbit", color = "#1b7f94")
    #   
    #   quant.msstats <- proteinSummarization(input_data, reference_norm = FALSE, use_log_file = FALSE)
    #   test.pairwise <- groupComparisonTMT(quant.msstats, moderated = TRUE, use_log_file = FALSE)
    #   
    #   remove_modal_spinner()
    #   
    #   test.pairwise
    #   
    # })
    
    output$table <- renderDT(input_data())
    
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