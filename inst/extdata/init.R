# Load packages
library(devtools)
library(dplyr)
library(bibliographica)
library(sorvi)

# Create the output directory if not yet exists
dir.create(output.folder)

if (!exists("mc.cores")) {

  mc.cores <- 1

}


print("Read raw data")

# Reading is slow, so save it as R object once ready
if (reload.data) {

  # Read the raw data
  df.orig <- read_bibliographic_metadata(fs,
		ignore.fields = ignore.fields,
		verbose = TRUE)

  # Save the raw data
  saveRDS(df.orig, file = "df.raw.Rds", compress = "xz")

} else {

  df.orig <- readRDS("df.raw.Rds")

}


# If update fields is provided, then look for previously preprocessed file
if (exists("update.fields") && !is.null(update.fields) && ("df0.Rds" %in% dir())) {

  df.preprocessed <- readRDS("df.Rds")    
  conversions <- readRDS("conversions.Rds")
  
} else {

  conversions <- list()
  update.fields <- names(df.orig) # Update all fields
  m <- setdiff(update.fields, c("control_number", "original_row", "language", "title", "publisher", "subject_topic", "publication_topic", "author_name", "author_date", "publication_time", "physical_extent", "physical_dimension", "publication_place", "publication_geography", "title_uniform", "title_uniform2"))
  if (length(m) > 0) {
    warning(paste("Updates not defined for the following input fields:", paste(m , sep = ";")))
  }
}


# Finally remove the specified fields
if (exists("ignore.fields")) {
  message("Ignoring fields: ")
  message(paste(update.fields, collapse = "; "))
  update.fields <- setdiff(update.fields, ignore.fields)
}



