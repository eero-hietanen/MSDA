# Modified 'MSTMTanalysis.R' script for FragPipe outputs.

cont = readline(prompt = "WARNING: This script clears your R environment. Do you want to continue? (y/n): ")

if(cont=="n")
  stop(simpleError("Script aborted by user."))

rm(list=ls())

# Constants for different parts of the script. Edit these if required:
# UniProt organism ID (TaxID) for the organism that was used in the experiment.

taxID = as.integer(readline(prompt = "Enter UniProt organism ID: "))

# Helper function for text formatting in message() output.
# From https://stackoverflow.com/a/45714700

tidymess <- function(..., prefix = " ", initial = ""){
  message(strwrap(..., prefix = prefix, initial = initial))
}

# Install pacman and BiocManager if necessary and use it to load other required packages.

tidymess("Checking for pacman and BiocManager installation.")

packages <- c("pacman", "BiocManager")
installed <- installed.packages()

for (package in packages) {
  if (!package %in% installed) {
    install.packages(package, dependencies = TRUE)
  }
}

tidymess("Installing other required packages.")
pacman::p_load(BiocManager, dplyr, stringr, MSstats, MSstatsTMT, MSstatsConvert, data.table, UniProt.ws, tcltk)

# Set working directory to the the script location.

tidymess("Working directory set to script source directory.")
setwd(utils::getSrcDirectory(function(){}))

# Load the evidence, proteinGroups, and annotation files located in the same
# folder as the script.

# 'fread()' function from the data.table package determines file headers and
# delimiters automatically. Alternatively, input files could be read in through,
# e.g., read.table("evidence,txt", sep="\t", header=TRUE), or using the 'readr'
# e.g. read_csv("file.csv", na = c("", "NA", "0")).

tidymess("Select your 'msstats.csv' and the annotation file.")

evidence <- tk_choose.files()
evidence <- fread(evidence)
annotation <- tk_choose.files()
annotation <- fread(annotation)

# SKIPPED FOR FRAGPIPE
# TODO: Check for userFasta presence in the evidence file. Abort if not found.
# tryCatch() here to check evidence as below with "Norm" channel
# 
# if (sum(str_detect(evidence$Proteins, userFasta)) == 0)
#   stop(simpleError("No entry found with user supplied .FASTA file name. Check 'evidence.txt' and your file name."))

# Convert FragPipe data to MSstatsTMT input. Preview the input through
# head(input) to check that everything looks right.

# Check that the 'Condition' column has at least two conditions set.
# Currently a bug(?) in FragPipe annotation file generation where it puts the
# conditions set in FragPipe in the BioReplicate column, and doesn't put anything
# in the condition column

if (sum(is.na(annotation$Condition)) > 0) {
  stop(simpleError("The condition column in the annotation file contains NA values."))
}

tidymess("Building MsStats input from MaxQuant files.")

MSstatsConvert::MSstatsLogsSettings(use_log_file = FALSE, pkg_name = "MSstatsTMT")
input <- PhilosophertoMSstatsTMTFormat(evidence, annotation, use_log_file = FALSE)

tidymess("----------------------head(input)-----------------------")
print(head(input))
tidymess("--------------------------------------------------------")

# Perform protein summarization and calculate statistics for all conditions
# in a pairwise manner. Empty reporter channels (marked as "Empty" condition in
# the annotation file) will be removed. If the conditions do not contain a
# normalization channel (marked as "Norm"), then reference normalization is not
# performed.

tidymess("Performing protein summarization and pairwise tests between conditions.")

if (sum(str_detect(input$Condition, '^Norm$')) > 0) {
  
  tidymess("Normalization channel found in 'Condition' annotation. Continuing
  with defaults.")
  
  quant.msstats <- proteinSummarization(input, use_log_file = FALSE)
  
} else {
  
  tidymess("Normalization channel not found in 'Condition' annotation; setting
  reference_norm to FALSE.")
  
  quant.msstats <- proteinSummarization(input, reference_norm = FALSE, use_log_file = FALSE)
}

tidymess("Performing pairwise comparisons using a moderated t-test. Use 
         'test.pairwise <- groupComparisonsTMT(quant.msstats)' to perform
         unmoderated testing.")

test.pairwise <- groupComparisonTMT(quant.msstats, moderated = TRUE, use_log_file = FALSE)

tidymess("----------head(test.pairwise$ComparisonResult)----------")
print(head(test.pairwise$ComparisonResult))
tidymess("--------------------------------------------------------")

# Filter comparison result table to drop rows with NA values (i.e. "Inf" and
# "-Inf" log2FC rows).

# Build accession list from comparison results and filter for unique IDs.
# Used regex matches UniProt accession numbers.

comparisonResult <- test.pairwise$ComparisonResult

# comparisonResult <- filter(comparisonResult, !is.na(comparisonResult$SE))
# 
# accessionList <- unique(as.vector(str_extract_all(comparisonResult$Protein,
#                                                   "[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}",
#                                                   simplify = TRUE)))
# 
# # Build UniProt.ws object with hamster database, query Uniprot and fetch
# # GO terms and protein descriptions for each accession. Full list of UniProtKB
# # return fields listed at https://www.uniprot.org/help/return_fields.
# # Chinese hamster taxon ID = 10029, obtained with 'availableUniprotSpecies(pattern="griseus")
# 
# taxDB <- UniProt.ws(taxID)
# uniprotResult <- UniProt.ws::select(x = taxDB, keys=accessionList, columns=c("protein_name", "go"))
# 
# # Merge UniProt results with existing comparison results.
# 
# tidymess("Merging UniProt information with group comparison table, renaming proteins, and tidying up.")
# 
# mergedResult <- NULL
# 
# if(userFasta!="") {
#   userFastaOutput <- comparisonResult[grepl(userFasta, comparisonResult$Protein)]
#   mergedResult <- rbind(mergedResult, userFastaOutput)
# }
# 
# for(row in 1:nrow(uniprotResult)) {
#   
#   output <- cbind(comparisonResult[grepl(uniprotResult[row,1], comparisonResult$Protein)], uniprotResult[row,])
#   
#   mergedResult <- rbind(mergedResult, output, fill = TRUE)
# }
# 
# # Rename 'Protein' column based on 'Entry' column and remove the redundant
# # 'Entry', 'From', and 'Issue' columns.
# # Done with dplyr if_else() as the base package ifelse had a renameing issue.
# 
# mergedResult$Protein <- if_else(is.na(mergedResult$Entry), mergedResult$Protein, mergedResult$Entry, mergedResult$Protein)
# mergedResult <- subset(mergedResult, select = -c(From,Entry, issue))
# 
# # Save tables of differentially expressed proteins for all group comparisons.
# 
# tidymess("Writing results table of differentially expressed proteins for all 
#          group comparisons.")
# 
# write.table(mergedResult,
#             file = "comparison_results.txt",
#             sep = "\t",
#             row.names = FALSE)
# 
# 
# tidymess("--------------------------------------------------------")
# tidymess("Done. Results table written to script folder under 'comparison_results.txt'.")
# tidymess("--------------------------------------------------------")

# TODO: Merging results has an issue with hits that involve multiple proteins where only the first is kept in the results
#   ->'accessionList' does not include the latter ones of multiprotein hits either
#   -> Just filtering out peptides with multiple protein hits is something people do
#     -> Interpreting PSMs with multiple hit targets is problematic as you can't tell which is the "real" protein match
# TODO: See about fixing the table writing if userFASTA == '' N.B. initialization of mergedResult as 'NULL' might have fixed it
# TODO: Export other tables, e.g. with abundance data?
# TODO: Look into how BioReplicates and channel names correspond in quant.msstats$ProteinLevelData with Bristol set labels. Also compare to annotation file.

# Volcano plots can be easily made with MSnSet.Utils package (see https://pnnl-comp-mass-spec.github.io/proteomics-data-analysis-tutorial/volcano-plots.html).
# E.g. plot_volcano(df=mergedResult[mergedResult$Label=="WT vs Neg"], logFC="log2FC", pvals="adj.pvalue", sig_threshold=0.05)
# MSstats also has a function to graph volcano plots, however it's not clear how it operates with MSstatsTMT generated group comparisons.

# Look into EnhancedVolcano package for plotting the volcano plots.

# Since services like PANTHER lack a database for Chinese hamster, GO analysis could be done with topGO package using a custom geneID<->GOid mapping
# as described in the topGO manual (Bioconductor site) section 4.3.