# dataupload handles uploading the initial evidence and annotation files
# as well as preprocessing the data to msstatstmt format

dataupload_ui <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(

    # add_busy_spinner(spin = "orbit", color = "#1b7f94"),

    fileInput(ns("evidence"), "Evidence"),
    fileInput(ns("annotation"), "Annotation"),
    actionButton(ns("preprocess"), "Preprocess files"),
    mainPanel(
    DTOutput(ns("table")),  # uncomment for table test
    )
    # 
    # mainPanel(
    #   textOutput(ns("text")) # testing for function output
    # )
  )

}

dataupload_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # myData <- reactiveValues()
    
    # observeEvent(input$preprocess, {
    #   show_modal_spinner(spin = "orbit", color = "#1b7f94")
    #   evid <- read.csv(input$evidence$datapath)
    #   annot <- read.csv(input$annotation$datapath)
    #   
    #   myData <- PhilosophertoMSstatsTMTFormat(evid, annot, use_log_file = FALSE)
    #   
    #   remove_modal_spinner()
    # })
    
    
    # If using observeEvent(), then toggle the use of reactiveValues and update
    # the reactive values inside the observed event. This is because observeEvent()
    # doesn't normally return vals, only reacts to an event with an action
    # N.B. the variable inside vals is given a name upon assignment, e.g., 'vals$myData'.
    # Use reactive values also with eventReactive() to pass, e.g., the raw data to another module
    
    # vals <- reactiveValues() # required in the event of observeEvent()
    
    rawData <- eventReactive(input$preprocess, {
      
      show_modal_spinner(spin = "orbit", color = "#1b7f94")

      req(list=c(input$evidence, input$annotation))
      
      evid <- read.csv(input$evidence$datapath)
      annot <- read.csv(input$annotation$datapath)
      
      dataOut <- PhilosophertoMSstatsTMTFormat(evid, annot, use_log_file = FALSE)
      # vals$myData <- dataOut # required with observeEvent(); can also be used with eventReactive() to pass reactive vals
      
      remove_modal_spinner()
      # If 'dataOut' is not called here after removal of the modal spinner, the table doesn't show up.
      # If spinner is not used, then the table shows normally without an additional call to the table
      dataOut
      # vals$myData <- dataOut
      
    })
    
    output$table <- renderDT(rawData()) # uncomment for table test
    # output$text <- renderPrint(vals$myData)
    
    # return(reactive(vals$myData))

    # return(val = reactive(rawData())) # this should also be fine returning just reactive(rawData())
    
  })
  
}

uploadTest <- function() {
  
  ui <- fluidPage(dataupload_ui("x"))
  server <- function(input, output, session){
    dataupload_server("x")
  }
  
  shinyApp(ui, server)
}
