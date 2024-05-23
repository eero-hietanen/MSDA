# Protein interaction network analysis with STRINGdb, igraph, and networkD3.
# Network module needs the table from plotting (UniProt table with significant hits) module as input.
# User should be able to:
#   1) Select a gene
#   2) View its interaction network
# Module's main view outputs should be the network graph and a data table (ideally Crosstalk linked).
# Data download should offer interaction partners of queried protein (other info? check STRINGdb website).

# Check the STRINGdb documentation for the Payload Mechanisms section w.r.t. colouring the nodes based on up-/down-reg.
# STRINGdb help page (API docs) https://string-db.org/cgi/help. Check the embedding section.
# TODO: Update the STRINGdb network to work based on the selection from the shared_data table
# TODO: See if there's a way to get back/select clusters from the network so that a selection could be done on the table based on the cluster,
#       so that the user can go back and examine how different clusters contain up-/down-regulated genes.
# TODO: Check clustering options for the network. Are they needed as settings for the network module?
# FIXME: There is a bug when plotting is done the first time where the Network module doesn't update the first selection the user does from the Plotting module?
#        Check if updating the network tab when the plotting is generated in the plotting tab would work. When switching to the network
#        tab the first time initialization of the inputs is seen briefly, so selection not working without first switching/updating the tab could be the reason.
# FIXME: Check the problem with the STRING network generation when multiple plot points are selected.

# jsCode <- "
#         shinyjs.loadStringData = function() {
#         getSTRING('https://string-db.org', {
#             'species': '9606',
#             'identifiers': 'TP53',
#             'network_flavor':'confidence'
#         });
#     };"

network_ui <- function(id) {
  
  ns <- NS(id)
  
  useShinyjs()
  # extendShinyjs(text = jsCode, functions = "loadStringData")
  
  # Something causing dysfunction here. Onclick for the button works, the extended js function doesn't seem to be called though.
  # Probabaly something to do with namespacing/modules? Try to put a print statement inside the 'loadStringData' to see if it gets called.
  # TODO: Look into implementing the loadStringData() JS function through Shiny.addCustomMessageHandler

  # extendShinyjs(text = paste0('shinyjs.loadStringData = function() {
  #       console.log("loadStringData called with gene:");
  #       getSTRING("https://string-db.org", {
  #           "ncbiTaxonId":"9606",
  #           "identifiers": gene,
  #           "network_flavor":"confidence"})
  #   }'), functions = "loadStringData")
  
  # tags$script(HTML("Shiny.addCustomMessageHandler('string_network_fetch', function(gene) {
  #     console.log('customMessageHandler loaded with', gene)
  #     getSTRING('https://string-db.org', {
  #           'species': '9606',
  #           'identifiers': [gene],
  #           'network_flavor':'confidence'
  #       });
  # });"))
  
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
        # STRINGdb seems to reliably recognize searches by gene names ('gene_primary' UniProt field)
        textInput(ns("gene_user"), label = "Genes", placeholder = "e.g.: TP53 MDM4"),
        numericInput(ns("node_count"), label = "Nodes", value = 10),
        sliderInput(ns("interaction_significance"), label = "Interaction threshold", min = 0, max = 1000, value = 0, step = 5),
        selectInput(ns("network_type"), label = "Network type", choices = list("Functional" = "functional", "Physical" = "physical"), selected = "Functional"),
        selectInput(ns("network_flavor"), label = "Network flavor", choices = list("Evidence" = "evidence", "Condifence" = "confidence", "Actions" = "actions"), selected = "Evidence"),
        numericInput(ns("species_id"), label = "Species taxa ID", value = "9606"),
        checkboxInput(ns("query_labels"), label = tooltip(
          trigger = list(
            "Use query labels as names",
            bs_icon("info-circle")
          ),
          "Use protein names from the data table as node names in the STRING network."
        ), value = FALSE),
        # checkboxInput(ns("colour_nodes"), label = tooltip(
        #   trigger = list(
        #     "Use colour in added nodes",
        #     bs_icon("info-circle")
        #   ),
        #   "Use coloured network nodes when adding extra nodes into the network."
        # ), value = FALSE),
        # Need an input selector for proteins (although should ideally just be based on selected data table row?)
        # What options does a STRINGdb search usually need? Target species, what sort of network to build etc.? Check their website
        # actionButton(ns("button"), "click"),
        fluidRow(
          actionButton(ns("build_network"), label = "Update network", width = "155px"),
          actionButton(ns("remove_nodes"), label = "-5", width = "auto"),
          actionButton(ns("add_nodes"), label = "+5", width = "auto"),
        style = 'display: flex; justify-content: space-around;'),
        actionButton(ns("network_selected"), label = "Network selected rows"),
        actionButton(ns("test_button"), label = "Test enrichment"),
        # downloadButton(ns("data_download"), "Download table"),
      )
    ),
    grid_card(
      area = "network_main",
      card_body(
        tags$div(id = "stringEmbedded"), # Not namespaced as the JS library doesn't find the namespaced element name
        # DTOutput(outputId = ns("network_table")),
        navset_underline(id = ns("network_main_tabs"),
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
    ))
}

network_server <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Evaluating data here means you can call rv$uniprot_data in renderDT without parenthesis
    # However, it means that the table is not immediately rendered when data becomes available
    # If you evaluate rv$uniprot_data in the renderDT, data shows up once it's available
    rv <- data
    rv$gene_user <- NULL
    
    # Network selected rows together with the user input genes.
    observe({
      gene_list <- fetch_selected_genes()
      
      rv$gene_list <- gene_list
      rv$gene_user <- input$gene_user
      rv$node_count <- input$node_count
      rv$interaction_significance <- input$interaction_significance
      rv$network_type <- input$network_type
      rv$network_flavor <- input$network_flavor
      rv$species_id <- input$species_id
      rv$query_labels <- as.integer(input$query_labels)
      
      additional_args <- build_args()
      rv$verbtext <- additional_args
      session$sendCustomMessage("string_network_fetch", additional_args)
      
    }) %>% bindEvent(input$network_selected)
    
    # Test button observe event. Use for testing enrichment.
    observe({
      selected_genes <- fetch_selected_genes() 
      user_genes <- input$gene_user
      genes <- c(selected_genes, user_genes)
      cat("selected ", selected_genes, "\n")
      cat("user ", user_genes, "\n")
      cat("combined ", genes, "\n")
      rv$enrichment <- string_api_call(genes)
    }) %>% bindEvent(input$test_button)
    
    # Observes the add_nodes button and increases the network nodes when clicked.
    observe({
      rv$node_count <- rv$node_count + 5
      additional_args <- build_args()
      updateNumericInput(session, "node_count", value = rv$node_count)
      session$sendCustomMessage('string_network_fetch', additional_args)
    }) %>% bindEvent(input$add_nodes)

    # Observes the remove_nodes button and decreases the network nodes when clicked.    
    observe({
      rv$node_count <- rv$node_count - 5
      additional_args <- build_args()
      updateNumericInput(session, "node_count", value = rv$node_count)
      session$sendCustomMessage('string_network_fetch', additional_args)
    }) %>% bindEvent(input$remove_nodes)
    
    # Tied to the "Update network" UI button. This should be merged with the "Network selected rows" so that one button does everything.
    observe({
      rv$gene_list <- input$gene_list
      rv$node_count <- input$node_count
      rv$interaction_significance <- input$interaction_significance
      rv$network_type <- input$network_type
      rv$network_flavor <- input$network_flavor
      rv$species_id <- input$species_id
      rv$query_labels <- as.integer(input$query_labels)
      
      additional_args <- build_args()
      session$sendCustomMessage("string_network_fetch", additional_args)
    }) %>% bindEvent(input$build_network)
    
    # Verbose text output used for monitoring variable values.
    output$verb_text_out <- renderPrint({
      req(!is.null(rv$verbtext))
      rv$verbtext
    })
    
    # Assigning a reactive value to a variable needs the RV to be evaluated with()
    # If calling for the RV inside the datatable() func, then it works without
    output$network_table <- renderDT({
      datatable(rv$t,
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
    }, server = FALSE)
    
    # Functional enrichment table output. Should return a parsed version of the STRING API call using httr2.
    # Currently could only be made to work with the user input. Table selection has a problem with reacing the actual protein names.
    output$enrichment_table <- renderDT({
      datatable(rv$enrichment,
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
    }, server = FALSE)
    
    # Fethces the selected genes from the table based on the selection.
    # It should retrieve the protein names from the "Protein" column, however it seems to just return row indices.
    # At the same time, things like the STRING DB API call to build the network correctly fethces the protein name when the call is sent.
    fetch_selected_genes <- function() {
      
      req(!is.null(rv$t))
      
      table <- rv$t
      selection <- table$selection() # returns an array of incides
      orig_data <- table$origData() # fetch original data that can be modified
      
      if (!is.null(selection) && length(selection) > 0) {
        # filtered <- subset(table, selection)$Protein
        filtered <- orig_data[selection, "Protein"] # Subsets the original data using indices from 'selection', restricts it to 'Protein' column. Essentially fetches the "Protein" column for selected rows.
        filtered <- as.character(filtered[[1]]) # This subsets and does a character vector conversion on the column, as the input is still a data.table and we want to only access the field values.
        rv$filtered <- filtered
      } else {
        filtered <- NULL
        rv$filtered <- filtered
      }
      
      filtered
    }
    
    # Builds a list of arguments that are passed to the STRING API call.
    # Fed to the JS function as an options object in order to access named arguments.
    build_args <- function() {
      
      additional_args <- list(
        gene_list = rv$gene_list,
        gene_user = rv$gene_user,
        node_count = rv$node_count,
        interaction_significance = rv$interaction_significance,
        network_type = rv$network_type,
        network_flavor = rv$network_flavor,
        species_id = rv$species_id,
        query_labels = as.integer(rv$query_labels)
      )
      
      additional_args
    }
    
  })
}