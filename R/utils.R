#######################################
# ----- MSstats data processing ----- #
#######################################

# Preprocess the data based on inputs from data upload server.
# Returns the processed data from PhilosophertoMSstatsTMTFormat.
# TODO: Add QC plots through dataProcessPlots()?

# TODO: Fix this to handle MaxQuant input. Requires additional protein_groups input file.
data_preprocessing <- function(evidence, annotation, source_type) {
  req(list = c(evidence, annotation, source_type))
  show_modal_spinner(spin = "orbit", text = "Processing...", color = "#0d6efd")

  evidence <- vroom(evidence$datapath)
  annotation <- vroom(annotation$datapath)

  if (source_type == "FragPipe") {
    dataOut <- PhilosophertoMSstatsTMTFormat(evidence, annotation, use_log_file = FALSE)
  } else if (source_type == "ProteomeDiscoverer") {
    dataOut <- PDtoMSstatsTMTFormat(evidence, annotation, use_log_file = FALSE)
  }

  remove_modal_spinner()

  dataOut
}

#########################################
# ----- MSstats group comparisons ----- #
#########################################

# Performs group comparisons and pairwise testing based on the preprocessed data.
# Requires the output of data_preprocessing().
# Returns a group comparison data frame.

data_groupcomparisons <- function(input, ...) {
  show_modal_spinner(spin = "orbit", text = "Processing...", color = "#0d6efd")

  args <- unlist(list(...))

  # Check if the proteinSummarization option 'maxQuantilieforCensored' does anything with the NA data filtering
  quant.msstats <- proteinSummarization(input,
    method = args[["summ_method"]],
    global_norm = as.logical(args[["summ_peptide_norm"]]),
    reference_norm = as.logical(args[["summ_protein_norm"]]),
    remove_norm_channel = as.logical(args[["summ_remove_norm"]]),
    remove_empty_channel = as.logical(args[["summ_remove_empty"]]),
    maxQuantileforCensored = as.numeric(args[["summ_maxquantilecensor"]]),
    use_log_file = FALSE
  )

  test.pairwise <- groupComparisonTMT(quant.msstats,
    moderated = as.logical(args[["group_modttest"]]),
    use_log_file = FALSE
  )

  remove_modal_spinner()

  test.pairwise$ComparisonResult
}

# #####################################
# # ----- Volcano plot (manual) ----- #
# #####################################
#
# # Volcano plot through ggplot2. Requires comparisonResult from group comparisons
# # Default cutoff = 0.05.
# # TODO: Check plotting through groupComparisonPlots()
#
# plotting_volcano <- function(input, ...) {
#
#   args <- unlist(list(...))
#
#   #set up a df for the plotting values
#   plotdf <- input
#   #add a categorical column for up/down regulated genes; default value "NS"
#   plotdf$diffexp <- "NS"
#   # Changed the log2FC vals from 0.5 to 0.6, also for xintercept below (geom_vline()); change back if wrong; check standard log2FC cutoff
#   plotdf$diffexp[plotdf$log2FC > as.numeric(args[["plot_fccutoff"]]) & plotdf$adj.pvalue < as.numeric(args[["plot_pcutoff"]])] <- "Up-regulated"
#   # as.numeric is done for the negative value because otherwise the plotting function breaks
#   plotdf$diffexp[plotdf$log2FC < -as.numeric(args[["plot_fccutoff"]]) & plotdf$adj.pvalue < as.numeric(args[["plot_pcutoff"]])] <- "Down-regulated"
#   #set up base plot; note to log-transform p-value
#   p <- ggplot(plotdf, aes(x=log2FC, y=-log10(adj.pvalue), col=factor(diffexp), text = plotdf$Protein)) + geom_point()
#   #add cutoff lines; note yintercept log-transform to count for y-axis log-transform above
#   p <- p + geom_vline(xintercept = c(-as.numeric(args[["plot_fccutoff"]]), as.numeric(args[["plot_fccutoff"]])), col="#c91010") + geom_hline(yintercept = -log10(as.numeric(args[["plot_pcutoff"]])), col="#c91010")
#   p <- p + labs(x = "log2FC", y = "adjusted p.value", title = args[["plot_title"]], color = "")
#   #adjust colour mapping
#   # p <- p + scale_color_manual(values=c("red", "black", "blue"), name = "Differential expression")
#
#   #names for DE genes can be toggled by adding another column to the 'plotdf' and
#   #copying the 'Label' based on filtering by the 'diffexp' value of UP/DOWN
#   #check 'ggrepel' library and the geom_text_repel() function for label placement
#
#   # Return a list with the plot p and the plotdf. Use plotdf as the data table and enable download signif. proteins through it.
#   return(list(p = p, plotdf = plotdf))
#   # p
# }

#####################################
# ----- Volcano plot (manual) ----- #
#     Crosstalk enabled plotting    #
#####################################

plotting_volcano <- function(input, ...) {
  args <- unlist(list(...))

  # Set up base plot; note to log-transform p-value
  p <- ggplot(input, aes(x = log2FC, y = -log10(adj.pvalue), col = factor(diffexp), text = input$Protein)) + geom_point()
  # Add plot labels
  p <- p + labs(x = "log2FC", y = "adj. p-value", title = args[["plot_title"]], color = "")
  # Add cutoff lines; note yintercept log-transform to count for y-axis log-transform above
  p <- p + geom_vline(xintercept = c(-as.numeric(args[["plot_fccutoff"]]), as.numeric(args[["plot_fccutoff"]])), col = "#960000", linetype = "dash", linewidth = 0.3) + geom_hline(yintercept = -log10(as.numeric(args[["plot_pcutoff"]])), col = "#960000", linetype = "dash", linewidth = 0.3)
  # Adjust colour mapping
  palette <- brewer.pal(n = 3, name = "RdBu")
  plot_colors <- c("Down-regulated" = palette[1], "Up-regulated" = palette[3], "NS" = palette[2])
  color_scale <- scale_color_manual(name = " Differential expression", values = plot_colors, limits = c("Down-regulated", "Up-regulated", "NS"))
  p <- p + color_scale

  # Names for DE genes can be toggled by adding another column to the 'plotdf' and
  # copying the 'Label' based on filtering by the 'diffexp' value of UP/DOWN
  # check 'ggrepel' library and the geom_text_repel() function for label placement

  # Return a list with the plot p and the plotdf. Use plotdf as the data table and enable download signif. proteins through it.
  return(list(p = p, plotdf = input)) # Could just return p here
}

##################################
# ----- UniProt data fetch ----- #
##################################

# Fetch GO terms (gene names) from UniProt. Should like be called from the "dataprocess" module.
# The table returned by this function should be reminiscent of the "merged_results"
# from the original R script. This table should also act as a basis for the plotting functions
# as it has more available data, such as gene names that can act as better labels for DE genes.

uniprot_fetch <- function(input, taxa_id, fields) {
  # Function to fetch UniProt data and construct a final results table similar to what's
  # in the original script file.
  # Requires 'taxa_id' and 'dataCols' as inputs to determine which organism to fetch the data for
  # and which UniProt fields should be fetched for the final table.

  show_modal_spinner(spin = "orbit", text = "Processing...", color = "#0d6efd")

  # Cat("Fetching with: ", taxa_id) # Test message to R console

  # Filter out the rows with found issues from group comparisons
  comparison_result <- filter(input, is.na(input$issue))

  # Drop the "issue" column as the rows have now been filtered out.
  # Using base R as it could be more efficient with large tables.
  # NOTE: There might be an argument for keeping the issue column in this table.
  # comparison_result$issue <- NULL

  accession_list <- unique(as.vector(str_extract_all(comparison_result$Protein,
    "[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}",
    simplify = TRUE
  )))

  # Build UniProt.ws object with species database, query Uniprot and fetch
  # GO terms and protein descriptions for each accession. Full list of UniProtKB
  # return fields listed at https://www.uniprot.org/help/return_fields.
  # Chinese hamster taxon ID = 10029, obtained with 'availableUniprotSpecies(pattern="greseus")

  taxa_db <- UniProt.ws(taxa_id)
  # Split fields character string by whitespace and unlist the result to retrieve a vector
  uniprot_result <- UniProt.ws::select(x = taxa_db, keys = accession_list, columns = unlist(strsplit(fields, " ")))

  # Merge UniProt results with existing comparison results.
  merged_result <- NULL

  # Simplify the Protein column in comparison_result, but keep the original for later use in step 2.
  comparison_result <- comparison_result %>%
    mutate(Original_Protein = Protein,
           Simplified_Protein = str_extract(Protein, "(?<=\\|)[^|]+(?=\\|)")) # Extracts the IDs from between the pipe symbols.

  # Merge the tables
  merged_result <- comparison_result %>%
    # Keep all entries from comparison_result. Drop "From" column from uniprot_result. Merge data by matching the simplified protein IDs with the entry IDs.
    left_join(select(uniprot_result, -From), by = c("Simplified_Protein" = "Entry")) %>%
    # Use coalesce to build the final Protein ID column. Uses Simplified_Protein ID if a match was found in the uniprot_results (non-NA column value), otherwise uses Original_Protein ID.
    mutate(Protein = coalesce(Simplified_Protein, Original_Protein)) %>%
    # Drop columns that are not needed for the final table.
    select(-Original_Protein, -Simplified_Protein, -DF, -issue)

  remove_modal_spinner()

  merged_result
}

####################################
# ----- UniProt species fetch -----#
####################################

uniprot_fetch_species <- function(input) {
  species_table <- UniProt.ws::availableUniprotSpecies(pattern = input)

  species_table
}

#######################################
# ----- UniProt field validation -----#
#######################################

uniprot_validate_fields <- function(input) {
  fields <- UniProt.ws::returnFields()
  cols <- unlist(strsplit(input, " "))
  fields_ok <- TRUE

  for (col in cols) {
    if (!(col %in% fields[["name"]])) {
      fields_ok <- FALSE
      break
    }
  }

  fields_ok
}


# TODO: Change this function to handle different API calls by modifying the req_url_path_append() function based an input value.
######################################################
# ----- STRINGdb API calls: Enrichment analysis -----#
# This can be modified to handle different api calls.#
# Pass a variable that is used to handle which API   #
# call is performed.                                 #
######################################################

string_api_call <- function(input, type_switch) {
  # Concatenates the identifiers. Carriage return, \r, is used as a delimiter for the func. enrichment API call.
  string_conv <- function(input) {
    paste0(input, collapse = "\r")
  }

  if (type_switch == "enrichment") {
    api_call_type <- "tsv/enrichment"
  } else if (type_switch == "url") {
    api_call_type <- "tsv/get_link"
  }

  req <- request("https://string-db.org/api")
  resp <- req |>
    req_url_path_append(api_call_type) |>
    req_url_query(identifiers = string_conv(input)) |>
    req_perform()
  resp_body_tsv <- resp |> resp_body_string()
  resp_body_tsv_parsed <- vroom(I(resp_body_tsv), delim = "\t")

  resp_body_tsv_parsed
}
