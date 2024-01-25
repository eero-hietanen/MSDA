# Preprocess the data based on inputs from data upload server.
# Returns the processed data from PhilosophertoMSstatsTMTFormat.

data_preprocessing <- function(evidence, annotation) {
  
  req(list=c(evidence, annotation))
  show_modal_spinner(spin = "orbit", color = "#1b7f94")
  
  MSstatsConvert::MSstatsLogsSettings(use_log_file = FALSE, pkg_name = "MSstatsTMT")
  evidence <- read.csv(evidence$datapath)
  annotation <- read.csv(annotation$datapath)
  dataOut <- PhilosophertoMSstatsTMTFormat(evidence, annotation, use_log_file = FALSE)
  
  remove_modal_spinner()
  
  dataOut
  
}

# Performs group comparisons and pairwise testing based on the preprocessed data.
# Requires the output of data_preprocessing().
# Returns a group comparison data frame.

data_groupcomparisons <- function(input) {
  
  show_modal_spinner(spin = "orbit", color = "#1b7f94")
  
  MSstatsConvert::MSstatsLogsSettings(use_log_file = FALSE, pkg_name = "MSstats")
  quant.msstats <- proteinSummarization(input, reference_norm = FALSE, use_log_file = FALSE)
  test.pairwise <- groupComparisonTMT(quant.msstats, moderated = TRUE, use_log_file = FALSE)
  
  remove_modal_spinner()
  
  test.pairwise$ComparisonResult
}