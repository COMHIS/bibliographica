# Load packages
library(devtools)
library(dplyr)
library(bibliographica)
library(sorvi)

# Reading is slow, so save it as R object once ready
# If update fields is provided, then look for previously preprocessed file


get_preprocessing_data <- function(df.orig, update.fields, ignore.fields, rewrite = FALSE) {

  message("Getting data for preprocessing.")

  if ("df0.Rds" %in% dir() & !rewrite) {
    message("Previously preprocessed data found in file df0.Rds.")
  }

  #if (exists("update.fields") && !is.null(update.fields) && ("data/unified/polished/df0.Rds" %in% dir())) {
  #
  #  message("Updating selected fields in previously saved data.")
  #  df.preprocessed <- readRDS("data/unified/polished/df.Rds")    
  #  conversions <- NULL # readRDS("conversions.Rds") TO REMOVE
  #  
  #} else {

    df.preprocessed <- NULL # as data is to be completely preprocessed
    conversions <- list()
    update.fields <- names(df.orig) # Update all fields
    message("Updating all fields: ")
    message(paste(update.fields, collapse = "; "))
    m <- update.fields
    if (length(m) > 0) {
      warning(paste("Updates not defined for the following input fields:", paste(m , sep = ";")))
    }
    
  #}

  # Finally remove the specified fields
  if (exists("ignore.fields")) {
    message("Ignoring fields: ")
    message(paste(ignore.fields, collapse = "; "))
    update.fields <- setdiff(update.fields, ignore.fields)
  }

  preprocessing.data <- list(df.preprocessed = df.preprocessed,
                             update.fields = update.fields,
                             conversions = conversions) 
  return(preprocessing.data)

}


