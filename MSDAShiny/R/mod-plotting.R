# plotting handles the volcano plots (check EnchancedVolcano lib) and
# saving the plots as a pdf/image file
# add a way to select plotted targets, e.g., full groups or just specific
# proteins (for QC plots)

plotting_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    selectInput(ns("group1"), "Choices", ""),
    actionButton(ns("plotvolc"), "Volcano plot of DE genes"),
    
  )
}

plotting_server <- function(id, groupcomparison_data) {
  
  moduleServer(id, function(input, output, session) {
    
    p <- NULL
    
    values <- groupcomparison_data #necessary?
    
    # g1Select = reactive({
    #   mydata = groupcomparison_data 
    #   names(mydata)
    # })
    # 
    # observe({
    #   updateSelectInput(session, "group1",
    #   choices = g1Select()
    # )})
    observe({
      p <- plotting_volcano(groupcomparison_data$groupcomp_data)
    }) %>% bindEvent(input$plotvolc)
    
    p
    
  })
}

plottingTest <- function() {
  
  ui <- fluidPage(plotting_ui("x"))
  server <- function(input, output, session){
    plotting_server("x", psumm)
  }
  
  shinyApp(ui, server)
}