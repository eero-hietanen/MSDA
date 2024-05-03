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
        textInput(ns("gene_list"), label = "Genes", value = "TP53"),
        numericInput(ns("node_count"), label = "Nodes", value = 10),
        sliderInput(ns("interaction_significance"), label = "Interaction threshold", min = 0, max = 1000, value = 0, step = 5),
        # FIXME: Add 'network_flavor' from the API
        selectInput(ns("network_type"), label = "Network type", choices = list("Functional" = "functional", "Physical" = "physical"), selected = "Functional"),
        numericInput(ns("species_id"), label = "Species taxa ID", value = "9606"),
        checkboxInput(ns("query_labels"), label = "Use query labels for nodes"),
        # Need an input selector for proteins (although should ideally just be based on selected data table row?)
        # What options does a STRINGdb search usually need? Target species, what sort of network to build etc.? Check their website
        # actionButton(ns("button"), "click"),
        fluidRow(
          actionButton(ns("build_network"), label = "Update network", width = "180px"),
          actionButton(ns("remove_nodes"), label = "-5", width = "auto"),
          actionButton(ns("add_nodes"), label = "+5", width = "auto"),
        style = 'display: flex; justify-content: space-around;'),
        actionButton(ns("network_selected"), label = "Network selected")
        # downloadButton(ns("data_download"), "Download table"),
      )
    ),
    grid_card(
      area = "network_main",
      card_body(
        tags$div(id = "stringEmbedded"), # Not namespaced as the JS library doesn't find the namespaced element name
        DTOutput(outputId = ns("network_table")),
      )
    )
  )
}

network_server <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # evaluating data here means you can call rv$uniprot_data in renderDT without parenthesis
    # however, it means that the table is not immediately rendered when data becomes available
    # if you evaluate rv$uniprot_data in the renderDT, data shows up once it's available
    rv <- data
    
    # onclick("button", { 
    #   # FIXME: fix namespacing, call js$loadStringData with input$gene
    #   js$loadStringData(ns("gene"))
    # })
    
    # observe({
    #   # geneid <- ns(input$gene)
    #   # cat(geneid)
    #   # js$loadStringData(ns(input$gene))
    #   js$loadStringData()
    # }) %>% bindEvent(input$button)
    
    
    # This works when a single gene is selected from the table.
    # If multiple are selected, the query is sent with the identifiers in a list, which breaks the getSTRING() call.
    # Check how to unlist/change the query to a regular vector that's accepted.
    observe({
      
      gene_list <- fetch_selected_genes()
      
      rv$gene_list <- gene_list
      rv$node_count <- input$node_count
      rv$interaction_significance <- input$interaction_significance
      rv$network_type <- input$network_type
      rv$species_id <- input$species_id
      rv$query_labels <- as.integer(input$query_labels)
      
      additional_args <- build_args()
      
      session$sendCustomMessage("string_network_fetch", additional_args)
      
    }) %>% bindEvent(input$network_selected)
    
    fetch_selected_genes <- function() {
      
      # req(!is.null(plots))
      # req(!is.null(input$plot_select) && input$plot_select != "")
      # req(!is.null(tables[[input$plot_select]]$selection()))
      req(!is.null(rv$t))
      
      table <- rv$t
      selection <- table$selection()
      table <- table$origData()
      
      filtered <- subset(table, selection)$Protein
      
      filtered
    }
    
    # assigning a reactive value to an variable needs the RV to be evaluated with()
    # if calling for the RV inside the datatable() func, then it works without
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
    
    build_args <- function() {
      
        additional_args <- list(
          gene_list = rv$gene_list,
          node_count = rv$node_count,
          interaction_significance = rv$interaction_significance,
          network_type = rv$network_type,
          species_id = rv$species_id,
          query_labels = as.integer(rv$query_labels)
        )
        
      additional_args
    }
    
    observe({
      rv$node_count <- rv$node_count + 5
      additional_args <- build_args()
      updateNumericInput(session, "node_count", value = rv$node_count)
      session$sendCustomMessage('string_network_fetch', additional_args)
    }) %>% bindEvent(input$add_nodes)
    
    observe({
      rv$node_count <- rv$node_count - 5
      additional_args <- build_args()
      updateNumericInput(session, "node_count", value = rv$node_count)
      session$sendCustomMessage('string_network_fetch', additional_args)
    }) %>% bindEvent(input$remove_nodes)

    onclick("build_network", {
      rv$gene_list <- input$gene_list
      rv$node_count <- input$node_count
      rv$interaction_significance <- input$interaction_significance
      rv$network_type <- input$network_type
      rv$species_id <- input$species_id
      rv$query_labels <- as.integer(input$query_labels)
      
      additional_args <- build_args()
      
      session$sendCustomMessage("string_network_fetch", additional_args)
    })
    
    # output$data_download <- downloadHandler(
    #   filename = function() {
    #     paste('network_data-', Sys.Date(), '.csv', sep="")
    #   },
    #   content = function(file) {
    #     write.csv(rv$uniprot_data(), file, row.names = FALSE)
    #   }
    # )
    
  })
  
}