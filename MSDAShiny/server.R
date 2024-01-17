library(shiny)

server <- function(input, output, session) {
  
  # uploadedData <- bindEvent(observe(input$uploadData, {
  #   # Data processing with uploaded file. Should execute as upload button is pressed.
  #   # Monitors uploadData button from UI input
  # })
  # )
  
  uploadedData <- eventReactive(input$uploadData, {
    
  })
}

# shinyApp(ui = ui, server = server) # Not needed on a multi-file setup
