# dataupload handles uploading the initial evidence and annotation files
# as well as preprocessing the data to msstatstmt format

dataupload_ui <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(

    # add_busy_spinner(spin = "orbit", color = "#1b7f94"),

    fileInput(ns("evidence"), "Evidence"),
    fileInput(ns("annotation"), "Annotation"),
    actionButton(ns("preprocess"), "Preprocess files"),
  )

}

dataupload_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # myData <- reactiveValues()
    
    observeEvent(input$preprocess, {
      show_modal_spinner(spin = "orbit", color = "#1b7f94")
      evid <- read.csv(input$evidence$datapath)
      annot <- read.csv(input$annotation$datapath)
      
      myData <- PhilosophertoMSstatsTMTFormat(evid, annot, use_log_file = FALSE)
      
      remove_modal_spinner()
      
      return(x="test")
    })
    
    
    
  })
  
}

uploadTest <- function() {
  
  ui <- fluidPage(dataupload_ui("x"))
  server <- function(input, output, session){
    dataupload_server("x")
  }
  
  shinyApp(ui, server)
}
