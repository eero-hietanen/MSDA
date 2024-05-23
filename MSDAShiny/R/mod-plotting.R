# plotting handles the volcano plots (check EnchancedVolcano lib) and
# saving the plots as a pdf/image file
# add a way to select plotted targets, e.g., full groups or just specific
# proteins (for QC plots)

# TODO: Implement accordions in the main panel. Put plot output in one, DT output in the other.
#       Link plot and DT so that selection <-> plot highlight work. Implement download of up/down reg. proteins.
# TODO: The plotting options for pval and FC cutoff need clarification regarding what the number is (e.g. some log of |2FC|?)
# TODO: Organize the plotting module code so that it has a clear division for crosstalk / non-crosstalk code for ease of testing.
# TODO: Add buttons/boxes to select all up-/down-regulated genes. Or both.
# FIXME: FC-cutoff and updating it is buggy on the plot; requires two clicks of the "Update plot" button to update

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
      accordion(
        # selectInput(ns("plot_select"), "Plot select", choices = NULL, selected = NULL),
        accordion_panel("Help",
        tags$div(HTML("<h5>Volcano plot</h5>
                      <p>Generate a volcano plot of based on the output from the data processing module. 
                      The plot is linked to the data table through Crosstalk.</p>
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
        accordion_panel("Plot options",
                        textInput(ns("plot_title"), label = "Title"),
                        # Use numericInput here instead of sliderInput as the p-val cutoff might have to be lowered substantially
                        numericInput(ns("plot_pcutoff"), label = "p-value cutoff", value = 0.05, step = 0.01),
                        numericInput(ns("plot_fccutoff"), label = "FC cutoff", value = 1.1, step = 0.1),
                        # sliderInput(ns("plot_pcutoff"), label = "p-value cutoff", min = 0, max = 1, value = 0.05),
                        # sliderInput(ns("plot_fccutoff"), label = "FC cutoff", min = 0, max = 3, value = 1.1, step = 0.1),
        ),
      ),
      tags$hr(),
      actionButton(ns("update_plot"),
                   "Update plot",
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
    
    observe({
      nav_select(id = "main_tabs", selected = "network")
      cat("clicked\n")
      # nav_select(id = "main_tabs", selected = "network")
    }) %>% bindEvent(input$tab_switch)
    
    # Each generated plot will need its own SDO (assign keys) and be linked to their respective plot.
    # Check if storing SDOs in th rvs is suitable and consider moving SDO generation inside the plot generation/data prep functions.
    # sharedDT <- SharedData$new(reactive(rv$prepped_data))

    # Call prep_data() to prep the data table by inserting NS col. and calculating significan hits based on fc and p cutoffs. Returns rv$prepped_data.
    # Generate a shared data object (currently reactive based on rv$prepped_data).
    # Call generate_plot and return a plot (don't actually need the plotdf from the plot func. anymore; can delete it).
    # DT render with the shared data object to enable Crosstalk functionality.
    # TODO: This should eventually work on the UniProt table instead of the groupcomp table.
    # TODO: Fix the plot highlight updating, i.e. when you first generate a plot, select a node, deselect it, then all of the plot points do not revert back to as being "selected", but remain semi-transparent.
    # observe({
    #   prep_data(rv$groupcomp_data()) # change to work with uniprot data
    #   generate_plot()
    #   shinyjs::show("data_download")
    # }) %>% bindEvent(input$update_plot)
    
    # FIXME: For FC-cutoff to update correctly it requires two clicks of the update plot button.
    observe({
      req(!is.null(rv$groupcomp_data()))
      generate_plot()
      shinyjs::show("data_download")
    }) %>% bindEvent(input$update_plot)
    
    # observe({
    #   req(!is.null(rv$groupcomp_data()))
    #   generate_plot()
    # }) %>% bindEvent(input$plot_fccutoff)
    # 
    # observe({
    #   req(!is.null(rv$groupcomp_data()))
    #   generate_plot()
    # }) %>% bindEvent(input$plot_pcutoff)
    
    # FIXME: The problem with the table updating with Crosstalk might be due to the way rv$prepped_data is used
    # If the shared data object refers back to prepped_data, then that'll always be the latest one
    # Test this and see if there's a way to generate a separate prepped_data for each plot/table pair
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

    # TODO: Consider removing the functionality of storing multiple plots and making them browsable through inputSelect.
    #       Instead, make the adjustments to FC and p-val cutoffs dynamic so they update in real time without generating a new plot
    #       and just have the single plot/table representation for the data.
    # FIXME: Changed to a single "real-time" output for the plot and table. However, it's somewhat fucked.
    #        Check the slider input and how the values are read / interpreted for the plot as they seem to bounce around.
    generate_plot <- function() {
      
      prep_data(rv$groupcomp_data())
      # When additional args are passed in this way, the varargs have to be unlisted in the plotting_volcano()
      # after which the varargs can be accessed by, e.g., args[["plot_title"]].
      # Another option is to use do.call(plotting_volcano, c(list(rv$groupcomp_data, additional_args))) which would
      # allow retrieving the varargs through just args <- list(...) in the utils func and then accessing the varargs with, e.g., args$plot_title.
      additional_args <- list(plot_title = input$plot_title,
                              plot_pcutoff = input$plot_pcutoff,
                              plot_fccutoff = input$plot_fccutoff)

      # counter(counter() +1) # Note how reactiveVal() is being updated compared to reactiveValues() element
      # plot_name <- paste0("Plot", counter())
      
      # 'group' is the parameter that acts as an identifier between the linked object groups, not 'key'
      # FIXME: A problem persists where the DT render output doesn't update to the correct data table (evident by 'diffexp' column values)
      shared_data <- SharedData$new(reactive(rv$prepped_data))
      
      plot_data <- plotting_volcano_test(shared_data, additional_args) # test plotting function used with Crosstalk
      
      rv$p <- plot_data[["p"]]
      rv$t <- shared_data
    
      # plots[[plot_name]] <- plot_data[["p"]]
      # # tables[[plot_name]] <- plot_data[["plotdf"]]
      # tables[[plot_name]] <- sharedDT
      # # Note the selection of the last element on the choices list by tail()
      # updateSelectInput(session, "plot_select", choices = names(plots), selected = tail(names(plots), 1))
    }

    # Crosstalk plot_table output
    # New table is generated alongside a new plot when the plotting button is pressed.
    # However, if navigating back, the table doesn't change based on selected plot.
    # Problem here seems to come from the SDO being updated to whatever the new values are for the table generation when pressing the button
    # and then that one SDO is used for all plots.
    # What's needed is a new SDO for each plot <-> table combination. Check Crosstalk functions for retrieving and cloning(?) the data.
    # Solution is likely related to assigning a key to each SDO when it's being linked to a plot (clone and assing key?)
    output$plot_table <- renderDT({
      # req(!is.null(plots))
      # req(!is.null(input$plot_select) && input$plot_select != "")
      req(!is.null(rv$t))
      
      plot_table <- rv$t
      datatable(
        plot_table,
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
      
      }, server = FALSE) # app struggles with large tables with client-side processing, but is required for Crosstalk to work

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
      # req(!is.null(plots))
      # req(!is.null(input$plot_select) && input$plot_select != "")
      req(!is.null(rv$p))

      ggplotly(rv$p) %>% highlight(on = 'plotly_selected', off = 'plotly_deselect') %>% config(scrollZoom = TRUE, modeBarButtonsToRemove = c('zoomIn', 'zoomOut', 'hoverCompareCartesian', 'hoverClosestCartesian')) %>% style(unselected=list(marker=list(opacity=1)))
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
    
    output$data_download <- downloadHandler(
      filename = function() {
        paste('significant_hits_data-', Sys.Date(), '.csv', sep="")
      },
      content = function(file) {
        table <- rv$t
        # This could use a drop of the 'issue' column, or just move to using UniProt table.
        significant_data <- subset(table$origData(), diffexp != "NS")
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