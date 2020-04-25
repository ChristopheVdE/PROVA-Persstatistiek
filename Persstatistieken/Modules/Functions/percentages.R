###############################################################################
# FUNCTION: Persberichten - Calcuate percentages
###############################################################################

# Calculate percentages =======================================================
calc_percentages <- function(Id, data) {
# Simple tables ---------------------------------------------------------------
  if (!(Id == "verzender.alg.beleid" || Id == "type" || Id == "return.beleid.alg" || Id == "return.beleid.beleid" || Id == "return.medium")) {
    total <- sum(data$Persberichten)
    column <- NULL
    for (i in 1:length(data$Persberichten)) {
      column <- c(column,
                 round(as.numeric(data$Persberichten[[i]] / total * 100), digits = 2))
    }
    return(column)
  } 
# Persberichten per beleid per verzender --------------------------------------
  else if (Id == "verzender.alg.beleid") {
    # Split dataframe on "Beleid
    df.split <- split(data, data$Verzender)
    # Create empty list
    column <- NULL
    # Calculate percentages
    for (verzender in levels(data$Verzender)) {
      percentages <- NULL
      for (i in 1:length(df.split[[verzender]]$Persberichten)) {
        percentages <- c(percentages,
                         round(as.numeric(df.split[[verzender]]$Persberichten[[i]] / sum(df.split[[verzender]]$Persberichten) * 100), digits = 2))
      }
      column <- c(column, percentages)
    }
    # Fix NA values
    for(i in 1:length(column)) {
      if (is.na(column[[i]])) {
        column[[i]] <- 0
      }
    }
    # Return
    return(column)
  }
# Persberichten per type ------------------------------------------------------
  else if (Id == "type") {
    # Split dataframe on "Beleid
    df.split <- split(data, data$Beleid)
    # Create empty list
    column <- NULL
    # Calculate percentages
    for (beleid in levels(data$Beleid)) {
      percentages <- NULL
      for (i in 1:length(df.split[[beleid]]$Persberichten)) {
        percentages <- c(percentages,
                         round(as.numeric(df.split[[beleid]]$Persberichten[[i]] / sum(df.split[[beleid]]$Persberichten) * 100), digits = 2))
      }
      column <- c(column, percentages)
    }
    # Fix NA values
    for(i in 1:length(column)) {
      if (is.na(column[[i]])) {
        column[[i]] <- 0
      }
    }
    # Return
    return(column)
  }
# Persreturn per beleid: algemeen ---------------------------------------------
  else if (Id == "return.beleid.alg") {
  # Split dataframe on "Beleid
    df.split <- split(data, data$Beleid)
  # Create empty list
    column <- NULL
  # Calculate percentages
    for (beleid in levels(data$Beleid)) {
      percentages <- NULL
      for (i in 1:length(df.split[[beleid]]$Aantal)) {
        percentages <- c(percentages,
                         round(as.numeric(df.split[[beleid]]$Aantal[[i]] / sum(df.split[[beleid]]$Aantal) * 100), digits = 2))
      }
      column <- c(column, percentages)
    }
  # Fix NA values
    for(i in 1:length(column)) {
      if (is.na(column[[i]])) {
        column[[i]] <- 0
      }
    }
  # Return
    return(column)
  }
# Persreturn per beleid: deelbeleid -------------------------------------------
  else if (Id == "return.beleid.beleid") {
  # Split dataframe on "Deelbeleid
    df.split <- split(data, data$Deelbeleid)
  # Create empty list
    column <- NULL
  # Calculate percentages
    for (deelbeleid in levels(data$Deelbeleid)) {
      percentages <- NULL
      for (i in 1:length(df.split[[deelbeleid]]$Aantal)) {
        percentages <- c(percentages,
                         round(as.numeric(df.split[[deelbeleid]]$Aantal[[i]] / sum(df.split[[deelbeleid]]$Aantal) * 100), digits = 2))
      }
      column <- c(column, percentages)
    }
  # Fix NA values
    for(i in 1:length(column)) {
      if (is.na(column[[i]])) {
        column[[i]] <- 0
      }
    }
  # Return
    return(column)
  }
# Persreturn per medium -------------------------------------------------------
  else if (Id == "return.medium") {
  # Split dataframe on "Deelbeleid
    df.split <- split(data, data$Beleid)
  # Create empty list
    column <- NULL
  # Calculate percentages
    for (beleid in levels(data$Beleid)) {
      percentages <- NULL
      for (i in 1:length(df.split[[beleid]]$Aantal)) {
        percentages <- c(percentages,
                         round(as.numeric(df.split[[beleid]]$Aantal[[i]] / sum(df.split[[beleid]]$Aantal) * 100), digits = 2))
      }
      column <- c(column, percentages)
    }
  # Fix NA values
    for(i in 1:length(column)) {
      if (is.na(column[[i]])) {
        column[[i]] <- 0
      }
    }
  # Return
    return(column)
  }  
}
# =============================================================================