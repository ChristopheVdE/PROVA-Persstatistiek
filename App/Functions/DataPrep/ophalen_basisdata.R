###############################################################################
# Name: Data preparation (function)
# FUCTION: prepare data for usage, clean dataset, remove un-neccessairy values
###############################################################################

# Load packages ===============================================================
library(readxl)
# =============================================================================

# FUNCTION ====================================================================
getbasisdata <- function(
                              file, 
                              sheet,
                              datarange) {
  # Prepare dataset -----------------------------------------------------------
  reactive({
    req(file())
    req(sheet())
    req(datarange())
    
    # Reading Excel -----------------------------------------------------------
    Excel <- read_excel(
                file(), 
                sheet = sheet(), 
                range = cell_cols(datarange())
             )
    
    # Return dataset ------------------------------------------------------------
    return(Excel)
  })
}