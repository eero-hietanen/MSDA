# Dataprocess handles viewing of the preprocessed data for checking by the user
# as well as starts rest of the data processing (protein summarization, group comparisons).

# TODO:
#  - Add options for using a reference channel
#  - Add a comparison selection function, i.e. if there are more groups than just standard 'control' and 'sample'
#  - QC plotting options
#  - Check https://laustep.github.io/stlahblog/posts/DTcallbacks.html for useful data table callbacks

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
      "320px",
      "1fr"
    ),
    gap_size = "3px",
    grid_card(
      area = "dataprocessing_side",
      full_screen = TRUE,
      card_header("Settings"),
      card_body(
        accordion(
          accordion_panel(
            "Group comparisons",
            selectInput(
              inputId = ns("summ_method"),
              label = "Summarization method",
              choices = list("MSstats" = "msstats", "Median Polish" = "MedianPolish", "Median" = "Median", "LogSum" = "LogSum")
            ),
            checkboxInput(inputId = ns("summ_peptide_norm"), label = tooltip(
              trigger = list(
                "Peptide level normalization",
                bs_icon("info-circle")
              ),
              "Global median normalization on peptide level data (equalizing the medians across all the channels and MS runs)."
            ), value = FALSE),
            checkboxInput(inputId = ns("summ_protein_norm"), label = tooltip(
              trigger = list(
                "Protein level normalization",
                bs_icon("exclamation-diamond-fill")
              ),
              "Reference channel based normalization between MS runs on protein level data. Needs at least one reference channel in each MS run, annotated by 'Norm' in Condtion column."
            ), value = FALSE),
            checkboxInput(inputId = ns("summ_remove_norm"), label = tooltip(
              trigger = list(
                "Remove normalization channel",
                bs_icon("info-circle")
              ),
              "Removes the normalization channel from protein level data."
            ), value = FALSE),
            checkboxInput(inputId = ns("summ_remove_empty"), label = tooltip(
              trigger = list(
                "Remove empty channels",
                bs_icon("info-circle")
              ),
              "Removes empty channels from protein level data."
            ), value = TRUE),
            checkboxInput(inputId = ns("group_modttest"), label = tooltip(
              trigger = list(
                "Use moderated t-test",
                bs_icon("info-circle")
              ),
              "Select to use a moderated t-test during statistical testing."
            ), value = FALSE),
            numericInput(ns("summ_maxquantilecensor"), label = tooltip(
              trigger = list(
                "Maximum quantile for censor",
                bs_icon("info-circle")
              ),
              "Censors missing values based on the maximum quantile value."
            ), value = 0.999, step = 0.01),
            actionButton(
              inputId = ns("groupcomparisons"),
              label = "Compare groups",
              width = "100%"
            ),
          ),
          accordion_panel(
            "UniProt search",
            textInput(inputId = ns("taxa_id"), label = tooltip(
              trigger = list(
                "Taxonomic ID or species name",
                bs_icon("info-circle")
              ), "UniProt taxa ID (numeric) or a species name to perform a search for taxa IDs (name can be partial). Note that genus names are capitalizied while species names are not."
            ), ),
            textInput(inputId = ns("uniprot_columns"), label = tooltip(
              trigger = list(
                "Additional UniProt fields",
                bs_icon("info-circle")
              ), "UniProt fields to add to the result table. Separated by a space, e.g. 'go xref_pdb protein_name'. Check 'UniProt help' link for a full list of fields."
            ), ),
            checkboxInput(inputId = ns("use_uniprot"), label = tooltip(
              trigger = list(
                "Use UniProt table",
                bs_icon("info-circle")
              ), "Use the table generated after fetching UniProt data in any further analysis steps. Helpful if you want to include additional UniProt fields in the result table."
            ), value = FALSE),
            actionButton(
              inputId = ns("uniprottable"),
              label = "Fetch UniProt data",
              width = "100%"
            ),
            # target="_blank" is required so that the link opens in a new browser tab.
            # Requires "Run External" option when app is launched from RStudio
            tags$a(href = "https://www.uniprot.org/help/return_fields", "UniProt help", target = "_blank"),
          ),
        ),
        tags$hr(),
        shinyjs::hidden(downloadButton(ns("data_download"), "Download table")),
        textOutput(ns("textout"))
      )
    ),
    grid_card(
      area = "dataprocessing_main",
      full_screen = TRUE,
      card_header("Data tables"),
      card_body(
        navset_underline(
          id = ns("dataprocess_tabs"),
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
}

dataprocess_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      additional_args <- list(
        summ_method = input$summ_method,
        summ_peptide_norm = input$summ_peptide_norm,
        summ_protein_norm = input$summ_protein_norm,
        summ_remove_norm = input$summ_remove_norm,
        summ_remove_empty = input$summ_remove_empty,
        summ_maxquantilecensor = input$summ_maxquantilecensor,
        group_modttest = input$group_modttest
      )

      data$groupcomp_data <- data_groupcomparisons(data$preprocessed_data, additional_args)
      shinyjs::show("data_download")
    }) %>% bindEvent(input$groupcomparisons)

    # Store the taxonomic ID for use later in the network module.
    observe({
      req(input$taxa_id)
      data$taxa_id <- input$taxa_id
    })

    # Observer for the UniProt search and subsequent table use.
    # If the UniProt search is done and the data is found, the the table will be set to be used in the next steps.
    observe({
      req(data$uniprot_data)
      updateCheckboxInput(session, "use_uniprot", value = TRUE)
    })

    observe({
      # Call util func to fetch uniprot data and construct table; returns the table
      req(data$groupcomp_data) # this could be changed to validate() to check if groupcomp_data is NULL or not

      if (input$taxa_id == "") { # Throw a warning if nothing is submitted
        showFeedbackWarning("taxa_id", "Taxonomic ID or at least partial species name is required")
        # Check for non numeric can be also done with is.na(as.numeric(input$taxa_id)) which returns TRUE if input is not numeric
      } else if (!grepl("^\\d+$", input$taxa_id)) { # if the input is not numeric, treat it as a species name pattern and fetch uniprot taxa IDs based on it
        hideFeedback("taxa_id")
        data$uniprot_species <- uniprot_fetch_species(input$taxa_id)
      } else { # Finally, if a numeric value is given use it as the taxa ID to fetch uniprot data; TODO: this should still be validated so that the user cannot submit alphanumeric values etc.
        # Call uniprot_validate_fields() to check the columns the user is about to call; show feedback if they're invalid
        # Validation here is buggy; check the utils function, likely a problem with the way input$uniprot_columns is unlisted and checked
        if (uniprot_validate_fields(input$uniprot_columns)) {
          hideFeedback("taxa_id")
          hideFeedback("uniprot_columns")
          data$uniprot_data <- uniprot_fetch(data$groupcomp_data, input$taxa_id, input$uniprot_columns)
        } else {
          showFeedbackWarning("uniprot_columns", "Wrong UniProt fields")
        }
      }
    }) %>% bindEvent(input$uniprottable)

    # Check whether the user wants to use the UniProt table for subsequent steps.
    observe({
      if (input$use_uniprot) {
        data$use_uniprot <- TRUE
      } else {
        data$use_uniprot <- FALSE
      }
    }) %>% bindEvent(input$use_uniprot)

    output$groupcomp_table <- renderDT({
      df <- datatable(
        data$groupcomp_data,
        rownames = FALSE,
        filter = "top",
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
      # Datatable modification from https://stackoverflow.com/a/66037552
      # shortens cells with characters > 30 and enables tooltip to view the cell data
      df <- datatable(
        data$uniprot_data,
        rownames = FALSE,
        filter = "top",
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
      df <- datatable(
        data$uniprot_species,
        rownames = FALSE,
        filter = "top",
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

    selected_tab <- reactive({
      input$dataprocess_tabs
    })

    output$data_download <- downloadHandler(
      filename = function() {
        if (selected_tab() == "Group comparisons") {
          file_name <- paste0("group_comp-data-", Sys.Date(), ".csv", sep = "")
        } else if (selected_tab() == "UniProt data") {
          file_name <- paste0("uniprot-data-", Sys.Date(), ".csv", sep = "")
        } else if (selected_tab() == "UniProt taxa IDs") {
          file_name <- paste0("uniprot-taxa_ids-", Sys.Date(), ".csv", sep = "")
        }
        return(file_name)
      },
      content = function(file) {
        get_data <- function() {
          if (selected_tab() == "Group comparisons") {
            data <- data$groupcomp_data
          } else if (selected_tab() == "UniProt data") {
            data <- data$uniprot_data
          } else if (selected_tab() == "UniProt taxa IDs") {
            data <- data$uniprot_species
          }
          return(data)
        }

        write.csv(get_data(), file, row.names = FALSE)
      }
    )
  })
}
