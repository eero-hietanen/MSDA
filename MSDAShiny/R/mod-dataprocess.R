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
# TODO: Group comparisons between other combinations than just control vs sample? Use factor() to get options from the right rable column?
# TODO: Group comparisons generating a data table with "Issue" column -> check where all it's used -> should be UniProt table (since it's formatted better)? -> if not, either way prune the Issues column and all invalid columns
# TODO: Insert a gap between tab titles (nav underline) and the actual data table
# TODO: Validate the UniProt columns that are fetched
# TODO: Check table generation functions: groupcomp table has row names (1,2,3...), while uniprot table doesn't

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
    gap_size = "3px",
    grid_card(
      area = "dataprocessing_side",
      full_screen = TRUE,
      card_header("Options"),
      card_body(
        accordion(
          accordion_panel(
            "Group comparisons",
            actionButton(
              inputId = ns("groupcomparisons"),
              label = "Compare groups",
              width = "100%"
            ),
          ),
          accordion_panel(
            "UniProt options",
            textInput(inputId = ns("taxID"), label = "TaxID", placeholder = "Taxa ID, or species name (can be partial)"),
            textInput(inputId = ns("uniprot_columns"), label = "UniProt Columns", placeholder = "e.g.: go protein_name xref_pdb"),
            actionButton(
              inputId = ns("uniprottable"),
              label = "Fetch UniProt data",
              width = "100%"
            ),
            # target="_blank" is required so that the link opens in a new browser tab.
            # Requires "Run External" option when app is launched from RStudio
            tags$a(href="https://www.uniprot.org/help/return_fields", "Open UniProt help", target="_blank"),
          ),
        ),
        tags$hr(),
        shinyjs::hidden(downloadButton(ns("data_download"), "Download table")),
        textOutput(ns("textout"))
      )
    ),
    # FIXME: The additional tables should become visible as necessary, and not from the start.
    grid_card(
      area = "dataprocessing_main",
      full_screen = TRUE,
      card_header("Data tables"),
      card_body(
        # bslib might have a solution for hidden panels.
        navset_underline(id = ns("dataprocess_tabs"),
          nav_panel(
            title = "Group comparisons",
            DTOutput(outputId = ns("groupcomp_table"), width = "100%"),
          ),
          nav_panel(
            title = "UniProt data",
            DTOutput(outputId = ns("uniprot_table"), width = "100%"),
          ),
          nav_panel(
            title = "UniProt taxa IDs",
            DTOutput(outputId = ns("uniprot_species"), width = "100%"),
          ),
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
      shinyjs::show("data_download")
    }) %>% bindEvent(input$groupcomparisons)  
    
    
    # observe({
    #   rv$groupcomp_data <- data_groupcomparisons(dataupload_data$preprocessed_data)
    #   shinyjs::show("data_download")
    # }) %>% bindEvent(input$groupcomparisons)
    
    # observe block gets triggered correctly, but the shinyjs::show function doesn't work
    # using showElement() works for enabling the taxID and uniprottable UI elements
    # check conditionalPanel for controlling the UI elements
    # observe({
    #   if (input$groupcomparisons > 0) {
    #     # cat("shinyjs button test")
    #     showElement("taxID")
    #     showElement("uniprottable")
    #     # shinyjs::show("taxID", asis = TRUE)
    #     # shinyjs::show("uniprottable", asis = TRUE)
    #   }
    # })
    
    observe({
      # call util func to fetch uniprot data and construct table; returns the table
      req(rv$groupcomp_data) # this could be changed to validate() to check if groupcomp_data is NULL or not
      
      if (input$taxID == "") { # throw a warning if nothing is submitted
        showFeedbackWarning("taxID", "Taxonomic ID or at least partial species name is required") 
        # Check for non numeric can be also done with is.na(as.numeric(input$taxID)) which returns TRUE if input is not numeric
      } else if (!grepl("^\\d+$", input$taxID)) { # if the input is not numeric, treat it as a species name pattern and fetch uniprot taxa IDs based on it
        hideFeedback("taxID")
        rv$uniprot_species <- uniprot_fetch_species(pattern = input$taxID)
      } else { # finally, if a numeric value is given use it as the taxa ID to fetch uniprot data; TODO: this should still be validated so that the user cannot submit alphanumeric values etc.
        hideFeedback("taxID")
        rv$uniprot_data <- uniprot_fetch(rv$groupcomp_data, input$taxID, input$uniprot_columns)
      }
    }) %>% bindEvent(input$uniprottable)
    
    output$groupcomp_table <- renderDT({df <- datatable(
        rv$groupcomp_data,
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
    
    output$uniprot_species <- renderDT({
      # datatable modification from https://stackoverflow.com/a/66037552
      # shortens cells with characters > 30 and enables tooltip to view the cell data
      df <- datatable(
        rv$uniprot_species,
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
    
    selected_tab <- reactive({input$dataprocess_tabs})
   
    output$data_download <- downloadHandler(
      filename = function() {
        if(selected_tab() == "Group comparisons") {
          file_name <- paste0('group_comp-data-', Sys.Date(), '.csv', sep="")
        }
        else if(selected_tab() == "UniProt data") {
          file_name <- paste0('uniprot-data-', Sys.Date(), '.csv', sep="")
        }
        else if(selected_tab() == "UniProt taxa IDs") {
          file_name <- paste0('uniprot-taxIDs-', Sys.Date(), '.csv', sep="")
        }
        return(file_name)
      },
      content = function(file) {
        
        get_data <- function() {
          if(selected_tab() == "Group comparisons") {
            data <- rv$groupcomp_data
          }
          else if(selected_tab() == "UniProt data") {
            data <- rv$uniprot_data
          }
          else if(selected_tab() == "UniProt taxa IDs") {
            data <- rv$uniprot_species
          }
          return(data)
        }
        
        write.csv(get_data(), file, row.names = FALSE)}
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