# dataupload handles uploading the initial evidence and annotation files
# as well as preprocessing the data to msstatstmt format

dataupload_ui <- function(id) {
  
  useShinyjs()
  ns <- NS(id)

  # Defining UI layout in the module instead of the main app. Call module UI inside the main app's tabPanel.
  tagList(
    sidebarPanel(
      fileInput(ns("evidence"), "Evidence"),
      fileInput(ns("annotation"), "Annotation"),
      actionButton(ns("preprocess"), "Preprocess files"),
    ),
    mainPanel(
      DTOutput(ns("preprocessed_table")),
      shinyjs::hidden(downloadButton(ns("data_download"), "Download data")),
    ),
  )
}

dataupload_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues(preprocessed_data = NULL)
    
    observe({
      rv$preprocessed_data <- data_preprocessing(input$evidence, input$annotation)
      showElement("data_download")
    }) %>% bindEvent(input$preprocess)

    
    output$preprocessed_table <- renderDT({
      datatable(
        rv$preprocessed_data,
        extensions = 'Buttons',
        options = list(
          autoWidth = TRUE,
          dom = 'Bfrtip',  # Include buttons extension
          buttons = c('csv', 'excel')  # Specify buttons for CSV and Excel
        )
      )
    })
    # reactive({
    #   list(preprocessed_data = rv$preprocessed_data)
    # })
    
    output$data_download <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep="")
      },
      content = function(file) {
        write.csv(rv$preprocessed_data, file)
      }
    )
    
    rv
    
  })
  
}

uploadTest <- function() {
  
  ui <- fluidPage(dataupload_ui("x"))
  server <- function(input, output, session){
    dataupload_server("x")
  }
  
  shinyApp(ui, server)
}
