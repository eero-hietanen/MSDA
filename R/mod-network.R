# Protein interaction network analysis with STRINGdb and the embedded JS library.
# TODO: There is a problem where a selection made on the Network page data table doesn't count for gene selection.
#       This has to do with the network DT render and the fetch_selected_genes function.

network_ui <- function(id) {
  ns <- NS(id)

  useShinyjs()

  grid_container(
    layout = c(
      "network_side network_main"
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
      area = "network_side",
      card_header("Settings"),
      card_body(
        textInput(ns("gene_user"), placehold = "e.g. TP53 MDM4", label = tooltip(
          trigger = list(
            "Additional query IDs",
            bs_icon("info-circle")
          ), "Protein/gene IDs to query STRINGdb with. IDs entered here can be queried separately if there is no selection made on the data table."
        ), ),
        numericInput(ns("node_count"), value = 0, label = tooltip(
          trigger = list(
            "Network node count",
            bs_icon("info-circle")
          ), "Sets the network node count if querying with a single protein. If the query consists of multiple proteins, only connections between queried proteins are shown."
        ), ),
        sliderInput(ns("interaction_significance"), value = 0.4, min = 0, max = 1, step = 0.05, label = tooltip(
          trigger = list(
            "Minimum interaction score",
            bs_icon("info-circle")
          ), "Minimum interaction score for connections to be shown in the network. STRINGdb defines the thresholds as: 0.15 = low confidence, 0.4 = medium confidence, 0.7 = high confidence, 0.9 = highest confidence."
        ), ),
        selectInput(ns("network_type"), label = "Network type", choices = list("Functional" = "functional", "Physical" = "physical"), selected = "Functional"),
        selectInput(ns("network_flavor"), label = "Network flavor", choices = list("Evidence" = "evidence", "Condifence" = "confidence", "Actions" = "actions"), selected = "Evidence"),
        textInput(ns("taxa_id"), label = "Taxonomic ID", placeholder = "e.g. 9606"),
        checkboxInput(ns("query_labels"), label = tooltip(
          trigger = list(
            "Use query labels as names",
            bs_icon("info-circle")
          ),
          "Use protein names from the data table as node names in the STRING network."
        ), value = FALSE),
        fluidRow(
          actionButton(ns("update_network"), label = "Update network", width = "155px"),
          actionButton(ns("remove_nodes"), label = "-5", width = "auto"),
          actionButton(ns("add_nodes"), label = "+5", width = "auto"),
          style = "display: flex; justify-content: space-around;"
        ),
        uiOutput(ns("network_url")),
        actionButton(ns("test_enrichment"), label = "Test enrichment"),
        # actionButton(ns("clear_selection"), label = "Clear selection"), # NYI
        # downloadButton(ns("data_download"), "Download table"), # NYI
      )
    ),
    grid_card(
      area = "network_main",
      card_body(
        tags$div(id = "stringEmbedded"), # Not namespaced as the JS library doesn't find the namespaced element name
        navset_underline(
          id = ns("network_main_tabs"),
          nav_panel(
            title = "Network data",
            DTOutput(outputId = ns("network_table"), width = "100%"),
          ),
          nav_panel(
            title = "Enrichment analysis",
            DTOutput(outputId = ns("enrichment_table"), width = "100%"),
          ),
          nav_panel(
            title = "Verbose text out",
            verbatimTextOutput(outputId = ns("verb_text_out")),
          ),
        )
      )
    )
  )
}

network_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Evaluating data here means you can call data$uniprot_data in renderDT without parenthesis
    # However, it means that the table is not immediately rendered when data becomes available
    # If you evaluate data$uniprot_data in the renderDT, data shows up once it's available
    # rv <- data
    # data$gene_user <- NULL

    # Reactive to update selected rows from the plotting DT.
    selected_rows <- reactive(data$selected_rows())

    # Network selected rows together with the user input genes.
    observe({
      if (input$taxa_id == "") { # Throw a warning if nothing is submitted
        showFeedbackWarning("taxa_id", "Taxonomic ID is required. If you don't know the ID, you can use the UniProt search on the data processing page to search for it based on a species name.")
        # Check for non numeric can be also done with is.na(as.numeric(input$taxa_id)) which returns TRUE if input is not numeric
      } else {
        hideFeedback("taxa_id")
        gene_list <- fetch_selected_genes()

        # data$gene_list <- gene_list #Disabled to test the ID mapping
        data$gene_user <- input$gene_user
        data$node_count <- input$node_count
        data$interaction_significance <- input$interaction_significance * 1000
        data$network_type <- input$network_type
        data$network_flavor <- input$network_flavor

        data$taxa_id <- as.integer(input$taxa_id) # Convert from text input.
        data$query_labels <- as.integer(input$query_labels)

        #Map gene/protein IDs to STRING IDs before building the args list
        data$gene_list <- string_api_id_mapping(gene_list)
        
        additional_args <- build_args()
        data$verbtext <- additional_args
        session$sendCustomMessage("string_network_fetch", additional_args)

        string_network_url_ids <- NULL
        
        data$call_type <- "url"
        
        string_api_args <- list(
          call_type = data$call_type,
          species = data$taxa_id
        )
        
        data$network_url <- string_api_call(data$gene_list, string_api_args)
      }
    }) %>% bindEvent(input$update_network)

    # Update the taxonomic ID if it's already been submitted in the data processing module.
    observe({
      req(data$taxa_id)
      updateTextInput(session, "taxa_id", value = data$taxa_id)
    })

    # Test button observe event. Use for testing enrichment.
    observe({
      gene_list <- fetch_selected_genes()
      gene_user <- input$gene_user
      data$call_type <- "enrichment"
      
      # cat("selected ", selected_genes, "\n")
      # cat("user ", user_genes, "\n")
      # cat("combined ", genes, "\n")
      
      string_enrichment_query_ids <- NULL
      string_api_args <- list(
        call_type = data$call_type,
        species = data$taxa_id
      )
      
      if (!is.null(gene_user) || gene_user == "") {
        string_enrichment_query_ids <- gene_list
      } else {
        string_enrichment_query_ids <- c(gene_list, gene_user)
      }
      
      data$enrichment <- string_api_call(string_enrichment_query_ids, string_api_args)
    }) %>% bindEvent(input$test_enrichment)

    # Observes the add_nodes button and increases the network nodes when clicked.
    observe({
      data$node_count <- data$node_count + 5
      additional_args <- build_args()
      updateNumericInput(session, "node_count", value = data$node_count)
      session$sendCustomMessage("string_network_fetch", additional_args)
    }) %>% bindEvent(input$add_nodes)

    # Observes the remove_nodes button and decreases the network nodes when clicked.
    observe({
      data$node_count <- data$node_count - 5
      additional_args <- build_args()
      updateNumericInput(session, "node_count", value = data$node_count)
      session$sendCustomMessage("string_network_fetch", additional_args)
    }) %>% bindEvent(input$remove_nodes)

    # Verbose text output used for monitoring variable values.
    output$verb_text_out <- renderPrint({
      req(!is.null(data$verbtext))
      data$verbtext
    })

    output$network_url <- renderUI({
      req(!is.null(data$network_url))
      network_url <- data$network_url
      a(href = network_url, target = "_blank", "Network URL")
    })

    ### Test for setting column filters as the selection for Crosstalk ->
    # Works, but also breaks the functionality of selecting rows yourself for analysis.

    # Observe filter changes and update Crosstalk selection
    observeEvent(input$network_table_search_columns, {
      req(input$network_table_search_columns)
      proxy <- dataTableProxy("table")

      # Get the indices of visible (filtered) rows
      proxy %>% selectRows(NULL)  # Clear existing selection
      proxy %>% selectRows(input$network_table_rows_all)

      # Update Crosstalk selection
      filtered_indices <- input$network_table_rows_all
      data$t$selection(filtered_indices)
    })
    ### <- Test

    # Assigning a reactive value to a variable needs the RV to be evaluated with()
    # If calling for the RV inside the datatable() func, then it works without
    output$network_table <- renderDT({

      req(!is.null(data$t))

      # Get the underlying data from the SharedData object
      full_data <- isolate(data$t$data())

      # Filter the data if there are selected rows
      if (!is.null(selected_rows()) && length(selected_rows()) > 0) {
        filtered_data <- full_data[selected_rows(), ]
      } else {
        filtered_data <- full_data
      }

      datatable(filtered_data,
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
    },
    server = FALSE
    )

    # Functional enrichment table output. Should return a parsed version of the STRING API call using httr2.
    # Currently could only be made to work with the user input. Table selection has a problem with reacing the actual protein names.
    output$enrichment_table <- renderDT({
      datatable(data$enrichment,
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
    },
    server = FALSE
    )

    # Fetches the selected genes from the table based on the selection.
    # It should retrieve the protein names from the "Protein" column, however it seems to just return row indices.
    # At the same time, things like the STRING DB API call to build the network correctly fetches the protein name when the call is sent.
    # TODO: This needs to be amended to use the data$selected_rows that's passed from the plotting module.
    fetch_selected_genes <- function() {
      req(!is.null(data$t))

      table <- data$t
      selection <- table$selection() # returns an array of incides
      orig_data <- table$origData() # fetch original data that can be modified

      if (!is.null(selection) && length(selection) > 0) {
        # filtered <- subset(table, selection)$Protein
        filtered <- orig_data[selection, "Protein"] # Subsets the original data using indices from 'selection', restricts it to 'Protein' column. Essentially fetches the "Protein" column for selected rows.
        filtered <- as.character(filtered[[1]]) # This subsets and does a character vector conversion on the column, as the input is still a data.table and we want to only access the field values.
        data$filtered <- filtered
      } else {
        filtered <- NULL
        data$filtered <- filtered
      }

      filtered
    }

    # Builds a list of arguments that are passed to the STRING API call.
    # Fed to the JS function as an options object in order to access named arguments.
    build_args <- function() {
      additional_args <- list(
        gene_list = data$gene_list,
        gene_user = data$gene_user,
        node_count = data$node_count,
        interaction_significance = data$interaction_significance,
        network_type = data$network_type,
        network_flavor = data$network_flavor,
        taxa_id = data$taxa_id,
        query_labels = as.integer(data$query_labels)
      )

      additional_args
    }
  })
}
