# TODO: Check if the currently used MSstats functions can be changed to the more base level ones (access with MSstats:::), so hopefully log file generation can be turned off properly.

# This app can be launched through the R console by running the following two commands:
# library(shiny)
# runApp("MSDA/app.R")

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
library(RColorBrewer)
library(vroom)
library(httr2)

options(shiny.maxRequestSize = 40 * 1024^2)
# options(shiny.error = NULL)
thematic_shiny()
ggplot2::theme_set(theme_minimal())

# tagList(
#   tags$head(
#     # CSS style edits with bslib, and directed to specific elements (e.g. datatables).
#     tags$style(".datatables td {padding-top: 1px; padding-bottom: 1px; font-size: 70%;}"),
#   )
# )


ui <- page_navbar(
  id = "main_tabs",

  # theme = bs_theme(bootswatch = "flatly"),

  # Loading things in the header due to bslib complaining otherwise and loading Shinyjs and feedback outside the UI doesn't work
  # Names of some UI elements can be found through browser inspect element function, e.g. to find 'navbar-brand'
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
    tags$script(src = "https://string-db.org/javascript/combined_embedded_network_v2.0.4.js"),

    # This cleans up and combines the identifiers that will be sent to STRINGdb.
    # The input supports either user inputs or selections from the plot, or both.
    tags$script(HTML("Shiny.addCustomMessageHandler('string_network_fetch', function(options) {

    if (options.gene_list !== null && options.gene_list !== '') {
        if (!Array.isArray(options.gene_list)) {
            options.gene_list = [options.gene_list];
        }
    } else {options.gene_list = [];}

    if (options.gene_user !== null && options.gene_user !== '') {
        options.gene_user = options.gene_user.split(' ');
        if (!Array.isArray(options.gene_user)) {
            options.gene_user = [options.gene_user];
        }
    } else {options.gene_user = [];}

    var combined_gene_list = [];

    if (options.gene_list.length > 0) {combined_gene_list = combined_gene_list.concat(options.gene_list);}
    if (options.gene_user.length > 0) {combined_gene_list = combined_gene_list.concat(options.gene_user);}

      console.log('STRINGdb identifiers:', combined_gene_list);

      getSTRING('https://string-db.org', {
        'species': options.taxa_id,
        'identifiers': combined_gene_list,
        'network_flavor': options.network_flavor,
        'network_type': options.network_type,
        'add_color_nodes': options.node_count,
        'required_score': options.interaction_significance,
        'show_query_node_labels': options.query_labels,
      });
    });")),
  ),

  # update params. for bs_theme_update were obtained through bs_themer().
  # also pipes to bs_add_rules() using |>, which is then used to update the CSS for, e.g., data tables
  # For fonts/icons etc., check https://fonts.google.com
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

  title = "seQwin.",
  nav_panel("Data upload", dataupload_ui("upload")),
  nav_panel("Data processing", dataprocess_ui("process")),
  nav_panel("Plotting", plotting_ui("plotting")),
  nav_panel("Network analysis", network_ui("network")),
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
  padding = "3px",
)

server <- function(input, output, session) {
  # bs_themer()

  data <- reactiveValues()

  dataupload_server("upload", data)
  dataprocess_server("process", data)
  plotting_server("plotting", data)

  # Hacky way to force switch to network tab when the shared data is generated from the plotting module.
  # Updates the namespace and links the SDO between the plotting and network modules.
  # observe({
  #   nav_select("main_tabs", "Network analysis")
  # }) %>% bindEvent(plotting_values$t)

  network_server("network", data)
}

shinyApp(ui, server)
