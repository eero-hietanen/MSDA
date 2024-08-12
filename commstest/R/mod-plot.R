plot_ui <- function(id) {
  
  useShinyjs()
  ns <- NS(id)
  
  tagList(
    actionButton(ns("make_plot"), "Plot"),
    plotOutput(ns("plot")),
  )
}

plot_server <- function(id, rv) {
  
  moduleServer(id, function(input, output, session) {
 
    plot_button_pressed <- reactiveVal(FALSE)  # Reactive value to track button press
    
    output$plot <- renderPlot({
      req(plot_button_pressed())  # Ensure the button is pressed before plotting
      data_set <- rv$selected_dataset()
      p <- ggplot(data_set)
    })
    
    observeEvent(input$make_plot, {
      plot_button_pressed(TRUE)  # Set the flag to indicate button press
    })
    
    observeEvent(rv$selected_dataset(), {
      plot_button_pressed(FALSE)  # Clear the flag when a new dataset is selected
    })
  })
}