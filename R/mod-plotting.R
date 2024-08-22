# Plotting handles the volcano plots (check EnchancedVolcano lib) and saving the plots as a pdf/image file

# TODO: Add buttons/boxes to select all up-/down-regulated genes. Or both.

plotting_ui <- function(id) {
  useShinyjs()
  ns <- NS(id)

  grid_container(
    layout = c(
      "plotting_side plotting_main_top",
      "plotting_side plotting_main_bottom"
    ),
    row_sizes = c(
      "3fr",
      "1fr"
    ),
    col_sizes = c(
      "320px",
      "1fr"
    ),
    gap_size = "3px",
    grid_card(
      area = "plotting_side",
      card_header = "Settings",
      card_body(
        accordion(open = FALSE,
          accordion_panel(
            "Help",
            tags$div(HTML("<h5>Volcano plot</h5>
                      <p>Generate a volcano plot based on the table from the data processing module.
                      The plot is linked to the data table using Crosstalk.</p>
                      <p>Basic plot tool buttons are found on the top right of the plot.
                      Both the plot and the data table can be expanded to full screen from the bottom right.</p>
                      <p>Basic functionality includes:
                      <ul>
                      <li>Zoom with mouse scroll</li>
                      <li>Box/lasso select points on the plot</li>
                      <li>Double click on the plot to de-select</li>
                      <li>Select data table rows to highlight plot data</li>
                      </ul></p>
                      ")),
          )
        ),
        accordion(
          accordion_panel(
            "Plot options",
            textInput(ns("plot_title"), label = "Title"),
            numericInput(ns("plot_pcutoff"), label = "p-value cutoff", value = 0.05, step = 0.01),
            numericInput(ns("plot_fccutoff"), label = "FC cutoff", value = 2, step = 0.1),
          ),
        ),
        tags$hr(),
        actionButton(ns("generate_plot"),
          "Generate plot",
          width = "100%"
        ),
        shinyjs::hidden(downloadButton(ns("data_download"), "Download table")),
      )
    ),
    grid_card(
      area = "plotting_main_top",
      full_screen = TRUE,
      card_body(
        plotlyOutput(ns("plot_output")),
      )
    ),
    grid_card(
      area = "plotting_main_bottom",
      full_screen = TRUE,
      card_body(
        DTOutput(outputId = ns("plot_table")),
      )
    )
  )
}

plotting_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    data$fccutoff <- reactive(input$plot_fccutoff)
    data$pcutoff <- reactive(input$plot_pcutoff)
    counter <- reactiveVal(0)

    plots <- reactiveValues()
    tables <- reactiveValues()

    observe({
      req(!is.null(data$groupcomp_data))
      shinyjs::show("data_download")
    }) %>% bindEvent(input$generate_plot)

    prep_data <- function(input) {
      fccutoff <- data$fccutoff()
      pcutoff <- data$pcutoff()

      # Add a categorical column for up/down regulated genes; default value "NS"
      input$diffexp <- "NS"
      # Changed the log2FC vals from 0.5 to 0.6, also for xintercept below (geom_vline()); change back if wrong; check standard log2FC cutoff
      input$diffexp[input$log2FC > fccutoff & input$adj.pvalue < pcutoff] <- "Up-regulated"
      # as.numeric is done for the negative value because otherwise the plotting function breaks
      input$diffexp[input$log2FC < -as.numeric(fccutoff) & input$adj.pvalue < pcutoff] <- "Down-regulated"

      data$prepped_data <- input
    }

    generate_plot <- function() {
      if (data$use_uniprot) {
        prep_data(data$uniprot_data)
      } else {
        prep_data(data$groupcomp_data)
      }
      # When additional args are passed in this way, the varargs have to be unlisted in the plotting_volcano()
      # after which the varargs can be accessed by, e.g., args[["plot_title"]].
      # Another option is to use do.call(plotting_volcano, c(list(data$groupcomp_data, additional_args))) which would
      # allow retrieving the varargs through just args <- list(...) in the utils func and then accessing the varargs with, e.g., args$plot_title.
      additional_args <- list(
        plot_title = input$plot_title,
        plot_pcutoff = input$plot_pcutoff,
        plot_fccutoff = input$plot_fccutoff
      )

      shared_data <- SharedData$new(reactive(data$prepped_data))
      data$shared_data <- shared_data

      plot_data <- plotting_volcano(shared_data, additional_args)

      data$p <- plot_data[["p"]]
      data$t <- shared_data
    }

    # Selected rows to be passed to the network module
    data$selected_rows <- reactive({
      req(data$t)
      data$t$selection()
    })

    # Crosstalk plot_table output
    output$plot_table <- renderDT({
      req(!is.null(data$t))

      plot_table <- data$t
      datatable(
        plot_table,
        rownames = FALSE,
        filter = "top",
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
    },
    server = FALSE
    ) # App struggles with large tables with client-side processing, but is required for Crosstalk to work

    output$plot_output <- renderPlotly({
      req(input$generate_plot)
      req(!is.null(data$groupcomp_data))

      # Generate plot called here to enable real time replotting upon FC/p-val changes.
      generate_plot()

      ggplotly(data$p) %>%
        highlight(on = "plotly_selected", off = "plotly_deselect") %>%
        config(scrollZoom = TRUE, modeBarButtonsToRemove = c("zoomIn", "zoomOut", "hoverCompareCartesian", "hoverClosestCartesian")) %>%
        style(unselected = list(marker = list(opacity = 1)))
    })

    output$data_download <- downloadHandler(
      filename = function() {
        paste("significant_hits_data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        table <- data$t
        # This could use a drop of the 'issue' column, or just move to using UniProt table.
        significant_data <- subset(table$origData(), diffexp != "NS")
        write.csv(significant_data, file, row.names = FALSE)
      }
    )
  })
}
