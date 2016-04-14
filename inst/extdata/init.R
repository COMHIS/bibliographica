# Load packages
library(devtools)
library(dplyr)
library(bibliographica)
library(sorvi)
# library(estc)

# Create the output directory if not yet exists
dir.create(output.folder)

# --------------------

print("Read raw data")

# Reading is slow, so save it as R object once ready
if (reload.data) {

  # Read the raw data
  df.orig <- read_bibliographic_metadata(fs, ignore.fields = ignore.fields, verbose = TRUE)

  # Save the raw data
  saveRDS(df.orig, file = "df.raw.Rds", compress = "xz")

} else {

  df.orig <- readRDS("df.raw.Rds")

}


# If update fields is provided, then look for preprocessed file
if (exists("update.fields") && !is.null(update.fields) && ("df0.Rds" %in% dir())) {
  df.orig <- df.orig # readRDS("df.raw.Rds")
  df.preprocessed <- readRDS("df0.Rds")  
  conversions <- readRDS("conversions.Rds")
} else {
  df.orig <- df.orig
  conversions <- list()
  update.fields <- names(df.orig) # Update all fields
  m <- paste(setdiff(names(df.orig), c("control_number", "original_row", "language", "title", "publisher", "subject_topic", "publication_topic", "author_name", "author_date", "publication_time", "physical_extent", "physical_dimension", "publication_place", "publication_geography", "title_uniform", "title_uniform2")), sep = ";")
  message(paste("Updates not defined for the following input fields:", m))

}







