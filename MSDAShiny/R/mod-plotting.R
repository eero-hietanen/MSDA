# plotting handles the volcano plots (check EnchancedVolcano lib) and
# saving the plots as a pdf/image file
# add a way to select plotted targets, e.g., full groups or just specific
# proteins (for QC plots)


plotting_ui <- function(id) {
  
  useShinyjs()
  ns <- NS(id)
  
  grid_container(
    layout = c(
      "plotting_side plotting_main"
    ),
    row_sizes = c(
      "1fr"
    ),
    col_sizes = c(
      "260px",
      "1fr"
    ),
    gap_size = "10px",
    grid_card(
      area = "plotting_side",
      card_header = "Settings",
      card_body(
        actionButton(ns("plotvolc"), "Volcano plot of DE genes"),
        actionButton(ns("plotvolc2"), "EnhancedVolcano plot"),
        selectInput(ns("plot_select"), "Plot select", choices = NULL, selected = NULL),
      )
    ),
    grid_card(
      area = "plotting_main",
      card_body(
        plotlyOutput(ns("plot_output")),
      )
    )
  
    # Easy way to generate a plot output if using the default renderPlot() in the server function. Doesn't work if using plotly.
    # grid_card_plot(
    #   area = "plotting_main",
    #   outputId = ns("plot_output"),
    # )
    
    
  )
  
  # tagList(
  #   sidebarPanel(
  #     actionButton(ns("plotvolc"), "Volcano plot of DE genes"),
  #     actionButton(ns("plotvolc2"), "EnhancedVolcano plot"),
  #     selectInput(ns("plot_select"), "Plot select", choices = NULL, selected = NULL),
  #   ),
  #   mainPanel(
  #     plotOutput(ns("plot_output")),
  #   ),
  # )
}

plotting_server <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(counter = 0, p1 = NULL, p2 = NULL)
    
    plots <- reactiveValues()
    
    generate_plot <- function() {
      rv$counter <- rv$counter + 1
      plot <- plotting_volcano(data$groupcomp_data) # This should likely be changed once more data plotting options are established
      plot_name <- paste("Plot", rv$counter)
      plots[[plot_name]] <- plot
      updateSelectInput(session, "plot_select", choices = names(plots))
    }
    
    observe({
      generate_plot()
    }) %>% bindEvent(input$plotvolc)
    
    # observe({
    #   rv$p1 <- plotting_volcano(data$groupcomp_data)
    # }) %>% bindEvent(input$plotvolc)
    
    # EnhancedVolcano() plotting. Works with the assembled results table generated through uniprot_fetch(), but not with
    # direct result table from group comparisons (complains about log2FC not being numeric).
    observe({
      rv$p2 <- plotting_volcano2(data$uniprot_data)
    }) %>% bindEvent(input$plotvolc2)
    
    output$plot_output <- renderPlotly({
      # FIXME: Find a way to validate the plot output so that it's not done if 
      #        there is no data to plot.
      # validate(
      #   need(is.null(plots), "Waiting for plots.")
      # )
      plot <- plots[[input$plot_select]]
      plot
    })
    
    # output$plot_output <- renderPlot(rv$p1)
    
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