library(shiny)
library(shinyjs)
library(tidyverse)

# jsCode <- "
#         shinyjs.loadStringData = function(gene) {
#         console.log('loadStringData called with gene:', gene);
#         getSTRING('https://string-db.org', {
#             'species': '9606',
#             'identifiers': gene,
#             'network_flavor':'confidence'
#         });
#     };"

ui <- fluidPage(
 
      useShinyjs(),
      
      # External JS library used to build a STRINGdb network graph.
      tags$script(src="https://string-db.org/javascript/combined_embedded_network_v2.0.4.js"),
      
  #     # Works. Note gene passed to getSTRING in an array (required by the function).
  #     tags$script(HTML("Shiny.addCustomMessageHandler('handler1', function(gene) {
  #     console.log('customMessageHandler loaded with', gene)
  #     var genes = gene.split(' ')
  #     getSTRING('https://string-db.org', {
  #           'species': '9606',
  #           'identifiers': genes,
  #           'network_flavor':'confidence',
  #           'add_color_nodes': '20',
  #       });
  # });")),
      
      # Works. Note gene passed to getSTRING in an array (required by the function).
      tags$script(HTML("Shiny.addCustomMessageHandler('handler1', function(options) {
      if (!Array.isArray(options.gene)) {options.gene = [options.gene];}
      console.log('cmh payload:', options.gene);
      getSTRING('https://string-db.org', {
            'species': options.speciesID,
            'identifiers': options.gene,
            'network_flavor':'confidence',
            'add_color_nodes': options.nodes,
        });
  });")),
      
      # extendShinyjs(text = jsCode, functions = "loadStringData"),

        mainPanel(
          textInput(inputId = "gene", label = "Gene", placeholder = "TP53, MDM4"),
          textInput(inputId = "gene2", label = "Gene", placeholder = "TP53, MDM4"),
          numericInput(inputId = "nodes", label = "Nodes", value = 10),
          numericInput(inputId = 'speciesID', label = 'Species', value=9606),
          # actionButton(inputId = "button1", "esj"),
          actionButton(inputId = "button2", "cmh"),
          actionButton(inputId = "more", "more"),
          actionButton(inputId = "less", "less"),
          actionButton(inputId = "test", "test"),
          tags$div(id = "stringEmbedded"),
        )
    )

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  combined_gene_input <- function() {
    
    rv$gene_list <- input$gene
    rv$genes_additional <- input$gene2

    if (!is.null(rv$gene_list) && !is.null(rv$genes_additional)) {
      gene_list <- c(rv$gene_list, rv$genes_additional)
    } else if (!is.null(rv$gene_list) && is.null(rv$genes_additional)) {
      gene_list <- rv$gene_list
    } else if (is.null(rv$gene_list) && !is.null(rv$genes_additional)) {
      gene_list <- rv$genes_additional
    } else {
      gene_list <- rv$gene_list
    }
    
    cat(gene_list)
    
  }
  
  observe({
    combined_gene_input()
  }) %>% bindEvent(input$test)
  
  # observe({
  #   rv$nodes <- input$nodes
  # }) %>% bindEvent(input$nodes)
  # 
  # observe({
  #   rv$nodes <- input$speciesID
  # }) %>% bindEvent(input$speciesID)
  # 
  # observe({
  #   rv$nodes <- input$gene
  # }) %>% bindEvent(input$gene)
  
  observe({
    rv$nodes <- rv$nodes + 10
    additional_args <- list(nodes = rv$nodes,
                            speciesID = rv$speciesID,
                            gene = rv$gene)
    session$sendCustomMessage('handler1', additional_args)
  }) %>% bindEvent(input$more)
  
  observe({
    rv$nodes <- rv$nodes - 10
    additional_args <- list(nodes = rv$nodes,
                            speciesID = rv$speciesID,
                            gene = rv$gene)
    session$sendCustomMessage('handler1', additional_args)
  }) %>% bindEvent(input$less)
  
  
  # onclick("button1", {
  #   req(input$gene)
  #   js$loadStringData(input$gene)
  # })
  
  # onclick("button2", {
  #   session$sendCustomMessage("handler1", input$gene)
  # })
  
  onclick("button2", {
    rv$nodes <- input$nodes
    rv$speciesID <- input$speciesID
    rv$gene <- input$gene
    if (!is.null(input$gene2)) {
      rv$gene <- c(rv$gene, input$gene2)
    }
    
    additional_args <- list(nodes = rv$nodes,
                            speciesID = rv$speciesID,
                            gene = rv$gene)
    session$sendCustomMessage("handler1", additional_args)
  })
  

}

shinyApp(ui = ui, server = server)
