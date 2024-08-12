library(shiny)
library(shinyjs)
library(crosstalk)
library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)



ui <- fluidPage(

        mainPanel(
          fileInput(
            inputId = "data",
            label = "Data",
          ),
          actionButton("make_plot", label = "Plot"),
          actionButton("select_test", label = "Select test"),
          selectInput("plot_select", "Plot select", choices = NULL, selected = NULL),
          plotlyOutput("plot"),
          DTOutput("table"),
          # Shows the logical table of selected rows. Use to filter data table, extract gene names (have to be UniProt or gene IDs?), and build STRINGdb
          verbatimTextOutput("selection_output"),
          verbatimTextOutput("selection_genes"),
          verbatimTextOutput("obs_selected"),
        )
)

server <- function(input, output, session) {
  
    rv <- reactiveValues()
    plots <- reactiveValues()
    tables <- reactiveValues()
    counter <- reactiveVal(0)
    
    observe({
      prep_data()
    }) %>% bindEvent(input$make_plot)
    
    
    # This example of Crosstalk works. Same has been achieved in the plotting module.
    # The 'key' is not the identifier for separating crosstalk linked groups, but 'groups' should do it.
    prep_data <- function() {
      
      # append counter and produce key
      counter(counter() +1)
      plot_name <- paste0("Plot", counter())
      
      # make a shared data object
      shared_data <- SharedData$new(rv$data(), group = plot_name)
      
      # make the plot using shared_data
      p <- ggplot(shared_data, aes(x=log2FC, y=-log10(adj.pvalue))) + 
                      geom_point() +
                      geom_vline(xintercept = c(-0.6, 0.6), col = "#c91010") + 
                      geom_hline(yintercept = -log10(0.05), col = "#c91010") +
                      labs(x = "log2FC", y = "pvalue", title = "Title", color = "")
      
      # put the plot and the table in the rvs
      plots[[plot_name]] <- p
      tables[[plot_name]] <- shared_data
      
      # rv$p <- p
      # rv$table <- shared_data
      
      # update selectinput
      updateSelectInput(session, "plot_select", choices = names(plots), selected = tail(names(plots), 1))
      
    }

    rv$data <- reactive({
      req(input$data)
      drop_na(read.csv(input$data$datapath))
      
    })
    
    output$selection_output <- renderText({
      req(!is.null(plots))
      req(!is.null(input$plot_select) && input$plot_select != "")
      table <- tables[[input$plot_select]]
      table$selection()
    })
    
    output$plot <- renderPlotly({
      req(!is.null(plots))
      req(!is.null(input$plot_select) && input$plot_select != "")
      plot <- plots[[input$plot_select]]
      ggplotly(plot) %>% highlight(color = "green", on = 'plotly_selected', off = 'plotly_deselect') %>% config(scrollZoom = TRUE, modeBarButtonsToRemove = c('zoomIn', 'zoomOut', 'hoverCompareCartesian', 'hoverClosestCartesian')) %>% style(unselected=list(marker=list(opacity=1))) %>% event_register("plotly_selected")
    })
    
    output$table <- renderDT({
      req(!is.null(plots))
      req(!is.null(input$plot_select) && input$plot_select != "")
      table <- tables[[input$plot_select]]
      datatable(table, options = list(pageLength = 10))
    }, server = FALSE)
    
    # output$selection_genes <- renderText({
    #   req(!is.null(plots))
    #   req(!is.null(input$plot_select) && input$plot_select != "")
    #   req(!is.null(tables[[input$plot_select]]$selection()))
    #   
    #   table <- tables[[input$plot_select]]
    #   selection <- table$selection()
    #   table <- table$origData()
    #   
    #   filtered <- subset(table, selection)$Protein
    #   
    #   filtered
    #   
    # })
    
    output$selection_genes <- renderPrint({
      req(!is.null(plots))
      req(!is.null(input$plot_select) && input$plot_select != "")
      req(!is.null(tables[[input$plot_select]]$selection()))
      
      table <- tables[[input$plot_select]]
      selection <- table$selection()
      table <- table$origData()
      
      # filtered <- subset(table, selection)$Protein
      filter <- input$table_rows_selected
      
      rv$names <- table[filter,]$Protein
      
      rv$names
      
    })
    
    # renderPrint used as renderText has a problem with rendering lists
    output$obs_selected <- renderPrint({
      d <- event_data("plotly_selected")
      if(is.null(d)) "TEST" else d
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
