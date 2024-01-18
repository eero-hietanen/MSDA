# plotting handles the volcano plots (check EnchancedVolcano lib) and
# saving the plots as a pdf/image file
# add a way to select plotted targets, e.g., full groups or just specific
# proteins (for QC plots)

plotting_ui <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(

  )
}

plotting_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}

plottingTest <- function() {
  
  ui <- fluidPage(plotting_ui("x"))
  server <- function(input, output, session){
    plotting_server("x")
  }
  
  shinyApp(ui, server)
}