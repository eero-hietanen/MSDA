# dataprocess handles viewing of the preprocessed data for checking by the user
# as well as starts rest of the data processing (protein summarization,
# group comparisons)
# once initial processing module works add a way to select different
# groups for comparisons

# TODO:
#  - Add options for using a reference channel
#  - Add a comparison selection function, i.e. if there are more groups than just
#    standard 'control' and 'sample'
#  - Add an export function for the UniProt data table
#  - QC plotting options?
#  - Adjustable cutoff values for VolcanoPlot (p-value and log2FC)
#  - VolcanoPlot image export
#  - Check https://laustep.github.io/stlahblog/posts/DTcallbacks.html for useful data table callbacks
#  - Modify the UI so that the plots sizing is sensible

dataprocess_ui <- function(id) {
  
  ns <- NS(id)

  fluidPage(
    textInput(ns("taxID"), "UniProt taxa ID or species name (can be partial)"),
    actionButton(ns("groupcomparisons"), "Perform group comparisons"),
    actionButton(ns("uniprottable"), "Fetch UniProt data or species taxa ID list"),
  )
}

# dataprocess_server needs myData from dataupload_server to further process the data

dataprocess_server <- function(id, dataupload_data) {

  moduleServer(id, function(input, output, session) {
    
    values <- reactiveValues(groupcomp_data = NULL, uniprot_data = NULL, uniprot_species = NULL)

    observe({
      values$groupcomp_data <- data_groupcomparisons(dataupload_data$preprocessed_data)
    }) %>% bindEvent(input$groupcomparisons)
    
    observe({
      # call util func to fetch uniprot data and construct table; returns the table
      req(values$groupcomp_data) # this could be changed to validate() to check if groupcomp_data is NULL or not
      
      if (input$taxID == "") { # throw a warning if nothing is submitted
        showFeedbackWarning("taxID", "Taxonomic ID or at least partial species name is required")  
      } else if (!grepl("^\\d+$", input$taxID)) { # if the input is not numeric, treat it as a species name pattern and fetch uniprot taxa IDs based on it
        hideFeedback("taxID")
        values$uniprot_species <- uniprotSpecies(pattern = input$taxID)
      } else { # finally, if a numeric value is given use it as the taxa ID to fetch uniprot data; TODO: this should still be validated so that the user cannot submit alphanumeric values etc.
        hideFeedback("taxID")
        values$uniprot_data <- uniprot_fetch(values$groupcomp_data, input$taxID)
      }
      
    }) %>% bindEvent(input$uniprottable)
    
    values
    
  })
  
}

processTest <- function() {

  myData <- iris
  ui <- fluidPage(dataprocess_ui("x"))
  server <- function(input, output, session){
    dataprocess_server("x", reactive({myData}))
  }

  shinyApp(ui, server)
}