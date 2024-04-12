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

# TODO: See if there's a way to add a raw console output to the app to monitor things like UniProt fetch in case there are server errors.
#       Also see about a way to interrupt the fetch (e.g. if it's stuck in an internal server error loop).

dataprocess_ui <- function(id) {
  
  useShinyjs()
  ns <- NS(id)

  grid_container(
    layout = c(
      "dataprocessing_side dataprocessing_main"
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
      area = "dataprocessing_side",
      full_screen = TRUE,
      card_header("Options"),
      card_body(
        actionButton(
          inputId = ns("groupcomparisons"),
          label = "Group compr."
        ),
        textInput(inputId = ns("taxID"), label = "TaxID", value = ""),
        actionButton(
          inputId = ns("uniprottable"),
          label = "Fetch UniProt data"
        )
      )
    ),
    # FIXME: The additional tables should become visible as necessary, and not from the start.
    grid_card(
      area = "dataprocessing_main",
      full_screen = TRUE,
      card_header("Table outputs"),
      card_body(
        tabsetPanel(
          nav_panel(
            title = "Group comparisons",
            DTOutput(outputId = ns("groupcomp_table"), width = "100%"),
            # FIXME: Figure out why these download buttons are acting differently compared to dataupload module.
            # shinyjs::hidden(downloadButton(ns("data_download2"), "Download table")),
          ),
          nav_panel(
            title = "UniProt taxa IDs",
            DTOutput(outputId = ns("uniprot_species"), width = "100%"),
          ),
          nav_panel(
            title = "UniProt data",
            DTOutput(outputId = ns("uniprot_table"), width = "100%"),
            # FIXME: Figure out why these download buttons are acting differently compared to dataupload module.
            # shinyjs::hidden(downloadButton(ns("data_download"), "Download table")),
          )
        )
      )
    )
  )

  
  # tagList(
  #   sidebarPanel(
  #     actionButton(ns("groupcomparisons"), "Perform group comparisons", style="margin-bottom: 5px;"),
  #     shinyjs::hidden(textInput(ns("taxID"), "UniProt taxa ID or species name (can be partial)")),
  #     shinyjs::hidden(actionButton(ns("uniprottable"), "Fetch UniProt data or species taxa ID list", style="margin-bottom: 5px;")),
  #   ),
  #   mainPanel(
  #     DTOutput(ns("groupcomp_table")),
  #     DTOutput(ns("uniprot_table")),
  #     DTOutput(ns("uniprot_species")),
  #   ),
  # 
  # )
}

# dataprocess_server needs myData from dataupload_server to further process the data

dataprocess_server <- function(id, dataupload_data) {

  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(groupcomp_data = NULL, uniprot_data = NULL, uniprot_species = NULL)

    observe({
      rv$groupcomp_data <- data_groupcomparisons(dataupload_data$preprocessed_data)
      showElement("data_download2")
    }) %>% bindEvent(input$groupcomparisons)
    
    # observe block gets triggered correctly, but the shinyjs::show function doesn't work
    # using showElement() works for enabling the taxID and uniprottable UI elements
    # check conditionalPanel for controlling the UI elements
    observe({
      if (input$groupcomparisons > 0) {
        # cat("shinyjs button test")
        showElement("taxID")
        showElement("uniprottable")
        # shinyjs::show("taxID", asis = TRUE)
        # shinyjs::show("uniprottable", asis = TRUE)
      }
    })
    
    observe({
      # call util func to fetch uniprot data and construct table; returns the table
      req(rv$groupcomp_data) # this could be changed to validate() to check if groupcomp_data is NULL or not
      
      if (input$taxID == "") { # throw a warning if nothing is submitted
        showFeedbackWarning("taxID", "Taxonomic ID or at least partial species name is required")  
      } else if (!grepl("^\\d+$", input$taxID)) { # if the input is not numeric, treat it as a species name pattern and fetch uniprot taxa IDs based on it
        hideFeedback("taxID")
        rv$uniprot_species <- uniprot_fetch_species(pattern = input$taxID)
      } else { # finally, if a numeric value is given use it as the taxa ID to fetch uniprot data; TODO: this should still be validated so that the user cannot submit alphanumeric values etc.
        hideFeedback("taxID")
        rv$uniprot_data <- uniprot_fetch(rv$groupcomp_data, input$taxID)
        # showElement("data_download3")
      }
    }) %>% bindEvent(input$uniprottable)
    
    output$groupcomp_table <- renderDT(rv$groupcomp_data)
    output$uniprot_table <- renderDT({
      # datatable modification from https://stackoverflow.com/a/66037552
      # shortens cells with characters > 30 and enables tooltip to view the cell data
      df <- datatable(
        rv$uniprot_data,
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
      })
    
    output$uniprot_species <- renderDT(rv$uniprot_species)
    
    output$data_download2 <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep="")
      },
      content = function(file) {
        # Extract the suffix from the selected tab ID
        selected_tab_suffix <- gsub(".*_(\\w+)$", "\\1", input$tabset_id)

        # Construct the corresponding data object name
        data_object_name <- paste0("preprocessed_data_", selected_tab_suffix)

        # Write the corresponding data table to CSV
        write.csv(get(data_object_name), file)
      }
    )
    
    rv
    
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