library(shiny)
library(shinyjs)
library(shinybusy)
library(shinyFeedback)
library(shinythemes)
library(MSstats)
library(MSstatsTMT)
library(MSstatsConvert)
library(data.table)
library(UniProt.ws)
library(DT)
library(bslib)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(plotly)
library(gridlayout)
library(thematic)
library(crosstalk)
library(colourpicker)
library(bsicons)
library(STRINGdb)
library(igraph)
library(vroom)
library(httr2)

options(shiny.maxRequestSize = 40 * 1024^2)

thematic_shiny()
ggplot2::theme_set(theme_minimal())

ui <- page_navbar(
  id = "main_tabs",

  header = tagList(
    useShinyjs(),
    useShinyFeedback(),
    tags$style(
      HTML("
      .navbar-brand {
      font-size: 40px;
      }
           ")
    ),
  ),

  theme = bs_theme_update(bs_theme(preset = "bootstrap"),
    base_font = font_google("Roboto"),
    font_scale = 0.9,
    `enable-rounded` = TRUE
  ) |> bs_add_rules(
    list(
      ".datatables td {padding-top: 3px; padding-bottom: 3px; font-size: 80%}",
      ".checkbox label {font-size: 90%; display:inline-block}"
    )
  ),

  title = "sync",
  nav_panel("Plot", plotting_ui("plotting")),
  nav_panel("Network analysis", network_ui("network")),
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
  padding = "3px",
  
)

server <- function(input, output, session) {

  data <- reactiveValues()
  plotting_server("plotting", data)
  network_server("network", data)

}

shinyApp(ui, server)