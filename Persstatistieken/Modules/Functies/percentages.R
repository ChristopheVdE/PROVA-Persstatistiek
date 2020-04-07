###############################################################################
# FUNCTION: Persberichten - Calcuate percentages
###############################################################################

# Calculate percentages =======================================================
calc_percentages <- function(data) {
  total <- sum(data$Persberichten)
  column <- NULL
  for (i in 1:length(data$Persberichten)) {
    column <- c(column,
               round(as.numeric(data$Persberichten[[i]] / total * 100), digits = 2))
  }
  return(column)
}
# =============================================================================