data_ui <- function(id) {
  
  useShinyjs()
  ns <- NS(id)
  
  tagList(
    
    selectInput(ns("data_sets"), "Select data set", choices=c("iris"), selected="iris"),
    DTOutput(ns("data_table")),
    actionButton(ns("showbtn"), "Show"),
    shinyjs::hidden(actionButton(ns("hidebtn"), "Hide")),
  )
  
}

data_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    data_records <- data(package="datasets")$results[,"Item"]
    data_names <- gsub("\\s+\\(.*\\)","", data_records)
    rv <- reactiveValues(data_names = data_names)
    
    observe({
      updateSelectInput(session, "data_sets", choices = rv$data_names)
    })
    
    # Define a reactive expression to retrieve the selected dataset
    rv$selected_dataset <- reactive({
      get(input$data_sets, envir = asNamespace("datasets")) #datasets here refers to the datasets package
    })
    
    # Render the datatable based on the reactive selected_dataset
    output$data_table <- renderDT({
      # Note: reactive values called as functions, e.g. rv$selected_dataset()
      if (!is.null(rv$selected_dataset())) {
        datatable(
          data.matrix(rv$selected_dataset()),
          selection = list(mode="multiple", target="column")
          )
      } else {
        NULL
      }
    })
    
    rv$selected_column <- reactive(input$data_table_columns_selected)
    
    # returning reactive values so that plot module can do something with the selected_dataset
    return(rv)
    
    
  })
}
