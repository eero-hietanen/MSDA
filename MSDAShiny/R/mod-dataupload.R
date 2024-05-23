# dataupload handles uploading the initial evidence and annotation files
# as well as preprocessing the data to msstatstmt format

dataupload_ui <- function(id) {
  
  useShinyjs()
  ns <- NS(id)

  grid_container(
    layout = c(
      "dataupload_side dataupload_main"
    ),
    row_sizes = c(
      "1fr"
    ),
    col_sizes = c(
      "320px",
      "1fr"
    ),
    gap_size = "3px",
    grid_card(
      area = "dataupload_side",
      card_header("Settings"),
      card_body(
        radioButtons(
          inputId = ns("source_type"),
          label = "Analysis program",
          choices = c("FragPipe", "MaxQuant", "ProteomeDiscoverer"),
          width = "100%",
        ),
        fileInput(
          inputId = ns("evidence"),
          label = "Evidence",
        ),
        fileInput(
          inputId = ns("annotation"),
          label = "Annotation",
        ),
        # If you're namespacing the uiOutput, then session$ns("id") part is needed in the server func. See https://gist.github.com/tbadams45/38f1f56e0f2d7ced3507ef11b0a2fdce
        # Also see https://stackoverflow.com/questions/56598005/how-to-use-shinyrenderui-and-shinyuioutput-to-produce-conditional-outputs-with
        uiOutput(ns("otheroptions")),
        actionButton(
          inputId = ns("preprocess"),
          label = "Preprocess"
        ),
        tags$hr(),
        shinyjs::hidden(downloadButton(ns("data_download"), "Download table")),
      )
    ),
    grid_card(
      area = "dataupload_main",
      card_body(
        DTOutput(outputId = ns("preprocessed_table"), width = "100%"),
        # shinyjs::hidden(downloadButton(ns("data_download"), "Download", style = "width: 110px; padding: 5px; position:absolute; bottom: 5px; right: 50%")),
      )
    )
  )
  
  # Defining UI layout in the module instead of the main app. Call module UI inside the main app's tabPanel.
  # tagList(
  #   sidebarPanel(
  #     fileInput(ns("evidence"), "Evidence"),
  #     fileInput(ns("annotation"), "Annotation"),
  #     actionButton(ns("preprocess"), "Preprocess files"),
  #   ),
  #   mainPanel(
  #     DTOutput(ns("preprocessed_table")),
  #     shinyjs::hidden(downloadButton(ns("data_download"), "Download data")),
  #   ),
  # )
}

dataupload_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues(preprocessed_data = NULL)
    
    output$otheroptions <- renderUI({
      if(reactive(input$source_type)() == "MaxQuant") { # Previous comparison error here was caused by 1) attempting to call without reactive(), and 2) calling the reactive object instead of the VALUE held in the reactive by using () at the end
        fileInput(
          inputId = session$ns("proteingroups"),  # note session$ns$("id")
          label = "Protein groups",
        )
      }
    })
    
    # Pass a third parameter to the data_preprocessing function which determines the function used for preprocessing (FP, MQ, PD)
    # Modify the data_preprocessing func in utils.R.
    # Also need to pass input$proteingroups if MQ is selected. Look into adding '...' to funcs to pass arguments. Look into 'do.call' and passing the arguments as a list. If the list is named based on the input$[name], then should be able to parse based on that in the utils function.
    observe({
      rv$preprocessed_data <- data_preprocessing(input$evidence, input$annotation, input$source_type)
      shinyjs::show("data_download")
    }) %>% bindEvent(input$preprocess)

    
    output$preprocessed_table <- renderDT({
      datatable(rv$preprocessed_data,
                rownames = FALSE,
                options = list(
                  scrollX = TRUE,
                  searching = TRUE,
                  pageLength = 25,
                  columnDefs = list(list(
                    targets = "_all",
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data != null && data.length > 30 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                      "}"
                    )
                  ))
                )
      )
    })
    # reactive({
    #   list(preprocessed_data = rv$preprocessed_data)
    # })
    
    output$data_download <- downloadHandler(
      filename = function() {
        paste('preprocessed_data-', Sys.Date(), '.csv', sep="")
      },
      content = function(file) {
        write.csv(rv$preprocessed_data, file, row.names = FALSE)
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
