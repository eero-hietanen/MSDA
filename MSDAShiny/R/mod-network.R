# Protein interaction network analysis with STRINGdb, igraph, and networkD3.
# Network module needs the table from plotting (UniProt table with significant hits) module as input.
# User should be able to:
#   1) Select a gene
#   2) View its interaction network
# Module's main view outputs should be the network graph and a data table (ideally Crosstalk linked).
# Data download should offer interaction partners of queried protein (other info? check STRINGdb website).

# Check the STRINGdb documentation for the Payload Mechanisms section w.r.t. colouring the nodes based on up-/down-reg.
# STRINGdb help page (API docs) https://string-db.org/cgi/help. Check the embedding section.

network_ui <- function(id) {
  
  ns <- NS(id)
  
  useShinyjs()
  # extendShinyjs(text = jsCode, functions = c("loadStringData"))
  
  # Something causing dysfunction here. Onclick for the button works, the extended js function doesn't seem to be called though.
  # Probabaly something to do with namespacing/modules? Try to put a print statement inside the 'loadStringData' to see if it gets called.

  extendShinyjs(text = paste0('shinyjs.loadStringData = function(gene) {
        console.log("loadStringData called with gene:", gene);
        var geneInput = Shiny.setInputValue(gene.id, gene)
        getSTRING("https://string-db.org", {
            "ncbiTaxonId":"9606",
            "identifiers": geneInput,
            "network_flavor":"confidence"})
    }'), functions = "loadStringData")
    
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
        textInput(ns("gene"), "Gene", "TP53"),
        # Need an input selector for proteins (although should ideally just be based on selected data table row?)
        # What options does a STRINGdb search usually need? Target species, what sort of network to build etc.? Check their website
        downloadButton(ns("data_download"), "Download table"),
        actionButton(ns("button"), "click"),
      )
    ),
    grid_card(
      area = "network_main",
      card_body(
        tags$div(id = ns("stringEmbedded")),
        # DTOutput(outputId = ns("network_table")),
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
    
    onclick("button", { 
      
    })
    
    # assigning a reactive value to an variable needs the RV to be evaluated with()
    # if calling for the RV inside the datatable() func, then it works without
    output$network_table <- renderDT({
      datatable(rv$uniprot_data(),
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
    
    output$data_download <- downloadHandler(
      filename = function() {
        paste('network_data-', Sys.Date(), '.csv', sep="")
      },
      content = function(file) {
        write.csv(rv$uniprot_data(), file, row.names = FALSE)
      }
    )
    
  })
  
}