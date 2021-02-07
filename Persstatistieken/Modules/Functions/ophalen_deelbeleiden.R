###############################################################################
# Name: Data preparation (function)
# FUCTION: prepare data for usage, clean dataset, remove un-neccessairy values
###############################################################################

# Load packages ===============================================================
library(readxl)
# =============================================================================

# FUNCTION ====================================================================
getdata.Deelbeleiden <- function(
                              file, 
                              sheet,
                              datarange) {
  # Prepare dataset -----------------------------------------------------------
  AlleDeelbeleiden <- reactive({
    req(file())
    req(sheet())
    req(datarange)
    
    # Reading Excel -----------------------------------------------------------
    Excel <- read_excel(
                file(), 
                sheet = sheet(), 
                range = cell_cols(datarange)
             )
    
    # Return dataset ------------------------------------------------------------
    return(Excel)
  })
}