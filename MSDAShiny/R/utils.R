# Preprocess the data based on inputs from data upload server.
# Returns the processed data from PhilosophertoMSstatsTMTFormat.

data_preprocessing <- function(evidence, annotation) {
  
  req(list=c(evidence, annotation))
  show_modal_spinner(spin = "orbit", color = "#1b7f94")

  evidence <- read_delim(evidence$datapath)
  annotation <- read_delim(annotation$datapath)
  dataOut <- PhilosophertoMSstatsTMTFormat(evidence, annotation, use_log_file = FALSE)
  
  remove_modal_spinner()
  
  dataOut
  
}

# Performs group comparisons and pairwise testing based on the preprocessed data.
# Requires the output of data_preprocessing().
# Returns a group comparison data frame.

data_groupcomparisons <- function(input) {
  
  show_modal_spinner(spin = "orbit", color = "#1b7f94")
  quant.msstats <- proteinSummarization(input, reference_norm = FALSE, use_log_file = FALSE)
  test.pairwise <- groupComparisonTMT(quant.msstats, moderated = TRUE, use_log_file = FALSE)
  
  remove_modal_spinner()
  
  test.pairwise$ComparisonResult
}

# Volcano plot through ggplot2. Requires comparisonResult from group comparisons
# Default cutoff = 0.05.

plotting_volcano <- function(input, cutoff = 0.05) {
  
  #set up a df for the plotting values
  plotdf <- input
  #add a categorical column for up/down regulated genes; default value "NS"
  plotdf$diffexp <- "NS"
  plotdf$diffexp[plotdf$log2FC > 0.5 & plotdf$adj.pvalue < 0.05] <- "UP"
  plotdf$diffexp[plotdf$log2FC < -0.5 & plotdf$adj.pvalue < 0.05] <- "DOWN"
  #set up base plot; note to log-transform p-value
  p <- ggplot(plotdf, aes(x=log2FC, y=-log10(adj.pvalue), col=diffexp)) + geom_point()
  #add cutoff lines; note yintercept log-transform to count for y-axis log-transform above
  p <- p + geom_vline(xintercept = c(-0.5, 0.5), col="red") + geom_hline(yintercept = -log10(0.05), col="red")
  #adjust colour mapping
  p <- p + scale_color_manual(values=c("red", "black", "blue"), name = "Differential expression")
  
  #names for DE genes can be toggled by adding another column to the 'plotdf' and
  #copying the 'Label' based on filtering by the 'diffexp' value of UP/DOWN
  #check 'ggrepel' library and the geom_text_repel() function for label placement
  
  p
}

# using EnhancedVolcano for plot generation
plotting_volcano2 <- function(input, cutoff = 0.05) {
  
  plotdf <- input
  
  EnhancedVolcano(plotdf, lab=plotdf$Protein, x="log2FC", y="adj.pvalue")
}

# Fetch GO terms (gene names) from UniProt. Should like be called from the "dataprocess" module.
# The table returned by this function should be reminiscent of the "merged_results"
# from the original R script. This table should also act as a basis for the plotting functions
# as it has more available data, such as gene names that can act as better labels for DE genes.

uniprot_fetch <- function(input, taxID) { # add dataCols input, which is a list of UniProt fields
  
  # function to fetch UniProt data and construct a final results table similar to what's
  # in the original script file.
  # requires 'taxID' and 'dataCols' as inputs to determine which organism to fetch the data for
  # what which UniProt fields should be fetched for the final table.
  
  show_modal_spinner(spin = "orbit", color = "#1b7f94")
  
  # cat("Fetching with: ", taxID) # Test message to R console
  
  comparisonResult <- filter(input, !is.na(input$SE))

  accessionList <- unique(as.vector(str_extract_all(comparisonResult$Protein,
                                                    "[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}",
                                                    simplify = TRUE)))

  # Build UniProt.ws object with hamster database, query Uniprot and fetch
  # GO terms and protein descriptions for each accession. Full list of UniProtKB
  # return fields listed at https://www.uniprot.org/help/return_fields.
  # Chinese hamster taxon ID = 10029, obtained with 'availableUniprotSpecies(pattern="greseus")

  taxDB <- UniProt.ws(taxID)
  # Columns below should be a set by user input
  uniprotResult <- UniProt.ws::select(x = taxDB, keys=accessionList, columns=c("protein_name", "go"))

  # Merge UniProt results with existing comparison results.

  mergedResult <- NULL

  for(row in 1:nrow(uniprotResult)) {

    output <- cbind(comparisonResult[grepl(uniprotResult[row,1], comparisonResult$Protein)], uniprotResult[row,])

    mergedResult <- rbind(mergedResult, output, fill = TRUE)
  }

  # Rename 'Protein' column based on 'Entry' column and remove the redundant
  # 'Entry', 'From', and 'Issue' columns.
  # Done with dplyr if_else() as the base package ifelse had a renameing issue.

  mergedResult$Protein <- if_else(is.na(mergedResult$Entry), mergedResult$Protein, mergedResult$Entry, mergedResult$Protein)
  mergedResult <- subset(mergedResult, select = -c(From,Entry, issue))
  
  remove_modal_spinner()
  
  mergedResult

}

