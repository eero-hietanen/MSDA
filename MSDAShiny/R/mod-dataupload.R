# dataupload handles uploading the initial evidence and annotation files
# as well as preprocessing the data to msstatstmt format

dataupload_ui <- function(id) {
  
  ns <- NS(id)

  tagList(
    # useShinyjs(),

    fileInput(ns("evidence"), "Evidence"),
    fileInput(ns("annotation"), "Annotation"),
    actionButton(ns("preprocess"), "Preprocess files"),
    actionButton(ns("groupcomparisons"), "Perform group comparisons"),
    DTOutput(ns("table")),  # uncomment for table test
    verbatimTextOutput(ns("text")) # testing for function output
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
    
    # rawData <- eventReactive(input$preprocess, {
    # 
    #   show_modal_spinner(spin = "orbit", color = "#1b7f94")
    # 
    #   req(list=c(input$evidence, input$annotation))
    # 
    #   evid <- read.csv(input$evidence$datapath)
    #   annot <- read.csv(input$annotation$datapath)
    # 
    #   dataOut <- PhilosophertoMSstatsTMTFormat(evid, annot, verbose = FALSE, use_log_file = FALSE) # verbose = FALSE cleans up the output, but the passed dataOut is still not accepted by the dataprocess server
    #   # vals$myData <- dataOut # required with observeEvent(); can also be used with eventReactive() to pass reactive vals
    # 
    #   remove_modal_spinner()
    #   # If 'dataOut' is not called here after removal of the modal spinner, the table doesn't show up.
    #   # If spinner is not used, then the table shows normally without an additional call to the table
    #   dataOut
    #   # vals$myData <- dataOut
    #   # return(dataOut)
    #   # browser()
    # })
    
    # output$table <- renderDT(data_processing(evidence = input$evidence, annotation = input$annotation)) # uncomment for table test
    # output$text <- renderPrint(rawData())
    
    # onclick("preprocess", data_processing(evidence = input$evidence, annotation = input$annotation))

    # return(vals = reactive(rawData())) # this should also be fine returning just reactive(rawData())
    
    # result <- reactive({
    #   req(list=c(input$evidecne, input$annotation))
    #   evidence <- read.csv(input$evidence$datapath)
    #   annotation <- read.csv(input$annotation$datapath)
    #   PhilosophertoMSstatsTMTFormat(evidence, annotation, use_log_file = FALSE)
    # }) %>% bindEvent(input$preprocess)
    
    # values <- reactiveValues(rawData = NULL)
    # 
    # preprocessed_data <- reactive({
    #   values$rawData <- data_preprocessing(input$evidence, input$annotation)
    #   values$rawData
    # }) %>% bindEvent(input$preprocess)
    # 
    # observe(preprocessed_data()) # observe() is required here to call result() which causes eventReactive() to be evaluated
    # reactive(preprocessed_data()) # last expression acts as a return; save 'return(x)' notation if returning 
    # 
    # groupcomp_data <- reactive({
    #   
    #   data_groupcomparisons(values$rawData)
    #   
    # }) %>% bindEvent(input$groupcomparisons)
    # 
    # observe(groupcomp_data())
    # reactive(groupcomp_data())
    
    values <- reactiveValues(preprocessed_data = NULL, groupcomp_data = NULL)
    
    observe({
      values$preprocessed_data <- data_preprocessing(input$evidence, input$annotation)
      values$preprocessed_data
    }) %>% bindEvent(input$preprocess)

    observe({
      values$groupcomp_data <- data_groupcomparisons(values$preprocessed_data)
    }) %>% bindEvent(input$groupcomparisons)
    
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
