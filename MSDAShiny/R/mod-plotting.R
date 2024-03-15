# plotting handles the volcano plots (check EnchancedVolcano lib) and
# saving the plots as a pdf/image file
# add a way to select plotted targets, e.g., full groups or just specific
# proteins (for QC plots)


plotting_ui <- function(id) {
  
  useShinyjs()
  ns <- NS(id)
  
  tagList(

    actionButton(ns("plotvolc"), "Volcano plot of DE genes"),
    actionButton(ns("plotvolc2"), "EnhancedVolcano plot"),
    
  )
}

plotting_server <- function(id, dataprocess_data) {
  
  moduleServer(id, function(input, output, session) {

    values <- reactiveValues(p1 = NULL, p2 = NULL)

    observe({
      values$p1 <- plotting_volcano(dataprocess_data$groupcomp_data)
    }) %>% bindEvent(input$plotvolc)
    
    observe({
      values$p2 <- plotting_volcano2(dataprocess_data$uniprot_data)
    }) %>% bindEvent(input$plotvolc2)
    
    values
    
  })
}

plottingTest <- function() {
  
  ui <- fluidPage(plotting_ui("x"))
  server <- function(input, output, session){
    plotting_server("x", psumm)
  }
  
  shinyApp(ui, server)
}