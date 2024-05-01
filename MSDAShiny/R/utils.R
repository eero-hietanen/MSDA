#######################################
# ----- MSstats data processing ----- #
#######################################

# Preprocess the data based on inputs from data upload server.
# Returns the processed data from PhilosophertoMSstatsTMTFormat.
# TODO: Add QC plots through dataProcessPlots()?

data_preprocessing <- function(evidence, annotation) {
  
  req(list=c(evidence, annotation))
  # TODO: Fix the background color of the busy popup / spinner element
  show_modal_spinner(spin = "orbit", text = "Processing...", color = "#0d6efd")

  evidence <- read_delim(evidence$datapath)
  annotation <- read_delim(annotation$datapath)
  dataOut <- PhilosophertoMSstatsTMTFormat(evidence, annotation, use_log_file = FALSE)
  
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
                                        reference_norm =  as.logical(args[["summ_protein_norm"]]),
                                        remove_norm_channel = as.logical(args[["summ_remove_norm"]]),
                                        remove_empty_channel = as.logical(args[["summ_remove_empty"]]),
                                        maxQuantileforCensored = as.numeric(args[["summ_maxquantilecensor"]]),
                                        use_log_file = FALSE)
  
  test.pairwise <- groupComparisonTMT(quant.msstats,
                                      moderated = as.logical(args[["group_modttest"]]),
                                      use_log_file = FALSE)
  
  remove_modal_spinner()
  
  test.pairwise$ComparisonResult
}

#####################################
# ----- Volcano plot (manual) ----- #
#####################################

# Volcano plot through ggplot2. Requires comparisonResult from group comparisons
# Default cutoff = 0.05.
# TODO: Check plotting through groupComparisonPlots()

plotting_volcano <- function(input, ...) {
  
  args <- unlist(list(...))
  
  #set up a df for the plotting values
  plotdf <- input
  #add a categorical column for up/down regulated genes; default value "NS"
  plotdf$diffexp <- "NS"
  # Changed the log2FC vals from 0.5 to 0.6, also for xintercept below (geom_vline()); change back if wrong; check standard log2FC cutoff
  plotdf$diffexp[plotdf$log2FC > as.numeric(args[["plot_fccutoff"]]) & plotdf$adj.pvalue < as.numeric(args[["plot_pcutoff"]])] <- "Up-regulated"
  # as.numeric is done for the negative value because otherwise the plotting function breaks 
  plotdf$diffexp[plotdf$log2FC < -as.numeric(args[["plot_fccutoff"]]) & plotdf$adj.pvalue < as.numeric(args[["plot_pcutoff"]])] <- "Down-regulated"
  #set up base plot; note to log-transform p-value
  p <- ggplot(plotdf, aes(x=log2FC, y=-log10(adj.pvalue), col=factor(diffexp), text = plotdf$Protein)) + geom_point()
  #add cutoff lines; note yintercept log-transform to count for y-axis log-transform above
  p <- p + geom_vline(xintercept = c(-as.numeric(args[["plot_fccutoff"]]), as.numeric(args[["plot_fccutoff"]])), col="#c91010") + geom_hline(yintercept = -log10(as.numeric(args[["plot_pcutoff"]])), col="#c91010")
  p <- p + labs(x = "log2FC", y = "adjusted p.value", title = args[["plot_title"]], color = "")
  #adjust colour mapping
  # p <- p + scale_color_manual(values=c("red", "black", "blue"), name = "Differential expression")
  
  #names for DE genes can be toggled by adding another column to the 'plotdf' and
  #copying the 'Label' based on filtering by the 'diffexp' value of UP/DOWN
  #check 'ggrepel' library and the geom_text_repel() function for label placement
  
  # Return a list with the plot p and the plotdf. Use plotdf as the data table and enable download signif. proteins through it.
  return(list(p = p, plotdf = plotdf))
  # p
}

## Testing function for plotting while Crosstalk is enabled ##

plotting_volcano_test <- function(input, ...) {
  
  args <- unlist(list(...))
  
  #set up base plot; note to log-transform p-value
  p <- ggplot(input, aes(x=log2FC, y=-log10(adj.pvalue), col=factor(diffexp), text = input$Protein)) + geom_point()
  #add plot labels
  p <- p + labs(x = "log2FC", y = "adj. p-value", title = args[["plot_title"]], color = "")
  #add cutoff lines; note yintercept log-transform to count for y-axis log-transform above
  p <- p + geom_vline(xintercept = c(-as.numeric(args[["plot_fccutoff"]]), as.numeric(args[["plot_fccutoff"]])), col="#960000", linetype = "dash", linewidth = 0.3) + geom_hline(yintercept = -log10(as.numeric(args[["plot_pcutoff"]])), col="#960000", linetype = "dash", linewidth = 0.3)
  #adjust colour mapping
  # p <- p + scale_color_manual(values=c("red", "black", "blue"), name = "Differential expression")
  
  #names for DE genes can be toggled by adding another column to the 'plotdf' and
  #copying the 'Label' based on filtering by the 'diffexp' value of UP/DOWN
  #check 'ggrepel' library and the geom_text_repel() function for label placement
  
  # Return a list with the plot p and the plotdf. Use plotdf as the data table and enable download signif. proteins through it.
  return(list(p = p, plotdf = input)) # Can just return p here
  # p
}

# ##############################################
# # ----- Volcano plot (EnhancedVolcano) ----- #
# ##############################################
# 
# plotting_volcano2 <- function(input, ...) {
#   
#   plotdf <- input
#   args <- list(...)
#   
#   EnhancedVolcano(plotdf, lab=plotdf$Protein, x="log2FC", y="adj.pvalue", title = args$plot_title, pCutoff = args$plot_pcutoff, FCcutoff = args$plot_fccutoff)
# }

##################################
# ----- UniProt data fetch ----- #
##################################

# Fetch GO terms (gene names) from UniProt. Should like be called from the "dataprocess" module.
# The table returned by this function should be reminiscent of the "merged_results"
# from the original R script. This table should also act as a basis for the plotting functions
# as it has more available data, such as gene names that can act as better labels for DE genes.

uniprot_fetch <- function(input, taxID, fields) {
  
  # function to fetch UniProt data and construct a final results table similar to what's
  # in the original script file.
  # requires 'taxID' and 'dataCols' as inputs to determine which organism to fetch the data for
  # what which UniProt fields should be fetched for the final table.
  
  show_modal_spinner(spin = "orbit", text = "Processing...", color = "#0d6efd")
  
  # cat("Fetching with: ", taxID) # Test message to R console
  
  # filter out the rows with found issues from group comparisons
  comparison_result <- filter(input, is.na(input$issue))

  accession_list <- unique(as.vector(str_extract_all(comparison_result$Protein,
                                                    "[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}",
                                                    simplify = TRUE)))

  # Build UniProt.ws object with species database, query Uniprot and fetch
  # GO terms and protein descriptions for each accession. Full list of UniProtKB
  # return fields listed at https://www.uniprot.org/help/return_fields.
  # Chinese hamster taxon ID = 10029, obtained with 'availableUniprotSpecies(pattern="greseus")

  taxDB <- UniProt.ws(taxID)
  # Split fields character string by whitespace and unlist the result to retrieve a vector
  uniprot_result <- UniProt.ws::select(x = taxDB, keys=accession_list, columns=unlist(strsplit(fields, " ")))

  # Merge UniProt results with existing comparison results.

  merged_result <- NULL

  for(row in 1:nrow(uniprot_result)) {

    output <- cbind(comparison_result[grepl(uniprot_result[row,1], comparison_result$Protein)], uniprot_result[row,])

    merged_result <- rbind(merged_result, output, fill = TRUE)
  }

  # Rename 'Protein' column based on 'Entry' column and remove the redundant
  # 'Entry', 'From', and 'Issue' columns.
  # Done with dplyr if_else() as the base package ifelse had a renaming issue.

  merged_result$Protein <- if_else(is.na(merged_result$Entry), merged_result$Protein, merged_result$Entry, merged_result$Protein)
  merged_result <- subset(merged_result, select = -c(From,Entry, issue))
  
  remove_modal_spinner()
  
  merged_result

}

####################################
# ----- UniProt species fetch -----#
####################################

uniprot_fetch_species <- function(input) {
  
  speciesTable <- UniProt.ws::availableUniprotSpecies(pattern = input)
  
  speciesTable
  
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
