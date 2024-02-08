# dataprocess handles viewing of the preprocessed data for checking by the user
# as well as starts rest of the data processing (protein summarization,
# group comparisons)
# once initial processing module works add a way to select different
# groups for comparisons

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
    
    values <- reactiveValues(groupcomp_data = NULL)
    
    # values <- reactiveValues(data=list(groupcomp_data = NULL, uniprot_table = NULL)) # initializing a list of reactiveValue elements; called as e.g. values$data$groupcomp_data
    
    observe({
      # test <- dataupload_data()[1] # note the way to call the reactiveValue in the list returned by dataupload
      # browser()
      values$groupcomp_data <- data_groupcomparisons(dataupload_data$preprocessed_data)
      # return(values)
    }) %>% bindEvent(input$groupcomparisons)
    
    observe({
      # call util func to fetch uniprot data and construct table; returns the table
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