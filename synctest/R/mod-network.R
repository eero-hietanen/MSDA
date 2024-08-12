network_ui <- function(id) {

ns <- NS(id)

  fluidPage(

    mainPanel(
        DTOutput(ns("table")),
        verbatimTextOutput(ns("verb_text_out")),
    )
  )
}

network_server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_rows <- reactive(data$selected_rows())

    output$table <- renderDT({
      req(!is.null(data$t))

      # Get the underlying data from the SharedData object
      full_data <- data$t$data()

      # Filter the data if there are selected rows
      if (!is.null(selected_rows()) && length(selected_rows()) > 0) {
        filtered_data <- full_data[selected_rows(), ]
      } else {
        filtered_data <- full_data
      }

      datatable(filtered_data, options = list(pageLength = 10))
    }, server = FALSE)

    output$verb_text_out <- renderPrint({
      req(!is.null(data$selected_rows))
      data$selected_rows()
    })
  })
}