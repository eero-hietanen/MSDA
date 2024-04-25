# plotting handles the volcano plots (check EnchancedVolcano lib) and
# saving the plots as a pdf/image file
# add a way to select plotted targets, e.g., full groups or just specific
# proteins (for QC plots)

# TODO: Implement accordions in the main panel. Put plot output in one, DT output in the other.
#       Link plot and DT so that selection <-> plot highlight work. Implement download of up/down reg. proteins.
# TODO: The plotting options for pval and FC cutoff need clarification regarding what the number is (e.g. some log of |2FC|?)

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
    # This side panel should probably be a sideBarPanel so that it's collapsible
    # To change the UI layout to a collapsible sidebar use layout_sidebar() and put the UI code starting from the
    # grid_card() element inside of it. Set grid_card() you want in the sidebar with sidebar = grid_card(...), and then
    # continue with the "main panel" grid_card().
    # This approach leaves out the grid_container section above.
    #  layout_sidebar(
    #     sidebar = grid_card(
    #         area = "plotting_side", ...
    
    grid_card(
      area = "plotting_side",
      card_header = "Settings",
      card_body(
        selectInput(ns("plot_select"), "Plot select", choices = NULL, selected = NULL),
      accordion(
        accordion_panel("Plot options",
                        textInput(ns("plot_title"), label = "Title"),
                        numericInput(ns("plot_pcutoff"), label = "p-value cutoff", value = 0.05, step = 0.01),
                        numericInput(ns("plot_fccutoff"), label = "FC cutoff", value = 0.6, step = 0.1),
        ),
      ),
      tags$hr(),
      actionButton(ns("make_plot"),
                   "Volcano plot",
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
    
    rv <- reactiveValues()
    
    rv$groupcomp_data <- reactive({data$groupcomp_data})
    rv$uniprot_data <- reactive({data$uniprot_data})
    rv$fccutoff <- reactive(input$plot_fccutoff)
    rv$pcutoff <- reactive(input$plot_pcutoff)
    counter <- reactiveVal(0)
    
    plots <- reactiveValues()
    tables <- reactiveValues()
    
    sharedDT <- SharedData$new(reactive(rv$prepped_data))

    # Call prep_data() to prep the data table by inserting NS col. and calculating significan hits based on fc and p cutoffs. Returns rv$prepped_data.
    # Generate a shared data object (currently reactive based on rv$prepped_data).
    # Call generate_plot and return a plot (don't actually need the plotdf from the plot func. anymore; can delete it).
    # DT render with the shared data object to enable Crosstalk functionality.
    # FIXME: There's an error (arg is of length 0) upon initialization because the DT render is trying to access the SDO which isn't generated yet(?)
    # TODO: This should eventually work on the UniProt table instead of the groupcomp table.
    # TODO: Fix multiple plot generation and Crosstalk links. Currently, generating multiple plots works, but the Crosstalk connection is only with plot1 and table1. The table output also doesn't update when changing plots selection.
    observe({
      prep_data(rv$groupcomp_data()) # change to work with uniprot data
      generate_plot()
      shinyjs::show("data_download")
    }) %>% bindEvent(input$make_plot)
    
    prep_data <- function(input) {
      
      prepped_data <- input
      
      fccutoff <- rv$fccutoff()
      pcutoff <- rv$pcutoff()

      #add a categorical column for up/down regulated genes; default value "NS"
      prepped_data$diffexp <- "NS"
      # Changed the log2FC vals from 0.5 to 0.6, also for xintercept below (geom_vline()); change back if wrong; check standard log2FC cutoff
      prepped_data$diffexp[prepped_data$log2FC > fccutoff & prepped_data$adj.pvalue < pcutoff] <- "Up-regulated"
      # as.numeric is done for the negative value because otherwise the plotting function breaks 
      prepped_data$diffexp[prepped_data$log2FC < -as.numeric(fccutoff) & prepped_data$adj.pvalue < pcutoff] <- "Down-regulated"
      
      rv$prepped_data <- prepped_data
    }
    
    # --- CROSSTALK TEST -->

    generate_plot <- function() {
      
      # When additional args are passed in this way, the varargs have to be unlisted in the plotting_volcano()
      # after which the varargs can be accessed by, e.g., args[["plot_title"]].
      # Another option is to use do.call(plotting_volcano, c(list(rv$groupcomp_data, additional_args))) which would
      # allow retrieving the varargs through just args <- list(...) in the utils func and then accessing the varargs with, e.g., args$plot_title.
      additional_args <- list(plot_title = input$plot_title,
                              plot_pcutoff = input$plot_pcutoff,
                              plot_fccutoff = input$plot_fccutoff)

      counter(counter() +1) # Note how reactiveVal() is being updated compared to reactiveValues() element
      
      plot_data <- plotting_volcano_test(sharedDT, additional_args) # test plotting function used with Crosstalk
      plot_name <- paste("Plot", counter())
      plots[[plot_name]] <- plot_data[["p"]]
      tables[[plot_name]] <- plot_data[["plotdf"]]
      # Note the selection of the last element on the choices list by tail()
      updateSelectInput(session, "plot_select", choices = names(plots), selected = tail(names(plots), 1))
    }

    # Crosstalk plot_table output
    output$plot_table <- renderDT({sharedDT}, server=FALSE)

    # <-- CROSSTALK TEST ---
     
    # generate_plot <- function() {
    #   additional_args <- list(plot_title = input$plot_title,
    #                           plot_pcutoff = input$plot_pcutoff,
    #                           plot_fccutoff = input$plot_fccutoff)
    #   counter(counter() +1) # Note how reactiveVal() is being updated compared to reactiveValues() element
    #   # When additional args are passed in this way, the varargs have to be unlisted in the plotting_volcano()
    #   # after which the varargs can be accessed by, e.g., args[["plot_title"]].
    #   # Another option is to use do.call(plotting_volcano, c(list(rv$groupcomp_data, additional_args))) which would
    #   # allow retrieving the varargs through just args <- list(...) in the utils func and then accessing the varargs with, e.g., args$plot_title.
    #   plot_data <- plotting_volcano(rv$groupcomp_data(), additional_args)
    #   plot_name <- paste("Plot", counter())
    #   plots[[plot_name]] <- plot_data[["p"]]
    #   tables[[plot_name]] <- plot_data[["plotdf"]]
    #   # Note the selection of the last element on the choices list by tail()
    #   updateSelectInput(session, "plot_select", choices = names(plots), selected = tail(names(plots), 1))
    # }

    output$plot_output <- renderPlotly({
      req(!is.null(plots))
      req(!is.null(input$plot_select) && input$plot_select != "")

      plot_plot <- plots[[input$plot_select]]
      plot_plot
    })

    #   output$plot_table <- renderDT({
    #   req(!is.null(plots))
    #   req(!is.null(input$plot_select) && input$plot_select != "")
    # 
    #   plot_table <- tables[[input$plot_select]]
    #   datatable(
    #     plot_table,
    #     rownames = FALSE,
    #     options = list(
    #       scrollX = TRUE,
    #       searching = TRUE,
    #       pageLength = 25,
    #       columnDefs = list(list(
    #         targets = "_all",
    #         render = JS(
    #           "function(data, type, row, meta) {",
    #           "return type === 'display' && data != null && data.length > 30 ?",
    #           "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
    #           "}"
    #         )
    #       ))
    #     )
    #   )
    # })

    
    # Edit this so that the used data table is the UniProt table with nicer protein names etc.
    output$data_download <- downloadHandler(
      filename = function() {
        paste('significant_data-', Sys.Date(), '.csv', sep="")
      },
      content = function(file) {
        significant_data <- subset(tables[[input$plot_select]], diffexp != "NS")
        write.csv(significant_data, file, row.names = FALSE)
      }
    )
    
    rv
    
  })
}

plottingTest <- function() {
  
  ui <- fluidPage(plotting_ui("x"))
  server <- function(input, output, session){
    plotting_server("x", psumm)
  }
  
  shinyApp(ui, server)
}