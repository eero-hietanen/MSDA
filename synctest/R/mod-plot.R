library(crosstalk)
plotting_ui <- function(id) {

ns <- NS(id)

fluidPage(

        mainPanel(
          fileInput(
            inputId = ns("data"),
            label = "Data",
          ),
          actionButton(ns("make_plot"), label = "Plot"),
          plotlyOutput(ns("plot")),
          DTOutput(ns("table")),
          # Shows the logical table of selected rows. Use to filter data table, extract gene names (have to be UniProt or gene IDs?), and build STRINGdb
        )
)
}

plotting_server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
    ns <- session$ns

    counter <- reactiveVal(0)
    
    observe({
      prep_data()
    }) %>% bindEvent(input$make_plot)
    
    
    # This example of Crosstalk works. Same has been achieved in the plotting module.
    # The 'key' is not the identifier for separating crosstalk linked groups, but 'groups' should do it.
    prep_data <- function() {
      
      req(data$data())

      # make a shared data object
      shared_data <- SharedData$new(data$data)
      
      # make the plot using shared_data
      p <- ggplot(shared_data, aes(x=log2FC, y=-log10(adj.pvalue))) + 
                      geom_point() +
                      geom_vline(xintercept = c(-0.6, 0.6), col = "#c91010") + 
                      geom_hline(yintercept = -log10(0.05), col = "#c91010") +
                      labs(x = "log2FC", y = "pvalue", title = "Title", color = "")
      
      # put the plot and the table in the rvs
      data$p <- p
      data$t <- shared_data

    }

    data$data <- reactive({
      req(input$data)
      drop_na(read.csv(input$data$datapath))
    })
    
    output$selection_output <- renderText({
      req(!is.null(data$p))
      req(!is.null(input$plot_select) && input$plot_select != "")
      table <- data$t
      table$selection()
    })

    output$plot <- renderPlotly({
      req(!is.null(data$p))
      plot <- data$p
      event_register(plot, "plotly_selected")
        ggplotly(plot) %>%
        highlight(on = "plotly_selected", off = "plotly_deselect") %>%
        config(scrollZoom = TRUE, modeBarButtonsToRemove = c("zoomIn", "zoomOut", "hoverCompareCartesian", "hoverClosestCartesian")) %>%
        style(unselected = list(marker = list(opacity = 1)))
    })

    output$table <- renderDT({
      req(!is.null(data$t))
      table <- data$t
      datatable(table, options = list(pageLength = 10))
    }, server = FALSE)

    # Capture selected rows and store the indices
    data$selected_rows <- reactive({
      req(data$t)
      data$t$selection()
    })

  })
}