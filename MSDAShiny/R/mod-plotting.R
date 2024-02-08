# plotting handles the volcano plots (check EnchancedVolcano lib) and
# saving the plots as a pdf/image file
# add a way to select plotted targets, e.g., full groups or just specific
# proteins (for QC plots)

plotting_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(

    actionButton(ns("plotvolc"), "Volcano plot of DE genes"),
    
  )
}

plotting_server <- function(id, groupcomparison_data) {
  
  moduleServer(id, function(input, output, session) {

    values <- reactiveValues(p = NULL)

    observe({
      values$p <- plotting_volcano(groupcomparison_data$groupcomp_data)
    }) %>% bindEvent(input$plotvolc)
    
    output$plot_output <- renderPlot({
      values$p
    })
    
  })
}

plottingTest <- function() {
  
  ui <- fluidPage(plotting_ui("x"))
  server <- function(input, output, session){
    plotting_server("x", psumm)
  }
  
  shinyApp(ui, server)
}