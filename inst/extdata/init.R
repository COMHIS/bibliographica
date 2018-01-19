# Load packages
library(devtools)
library(dplyr)
library(bibliographica)
library(sorvi)

# Create the output directory if not yet exists
# dir.create(output.folder)

# global variables in estc main.R when init.R called:
# output.folder <- "output.tables/"
# fs <- "estc.csv.gz"
# catalog <- "estc"
# languages <- c("english", "latin")
# mc.cores <- 4
# update.fields <- NULL
# ignore.fields <- c("title_uniform", "title_uniform2") # ESTC
# reload.data <- FALSE

# at end of init.R, also:
# df.orig
# df.preprocessed
# conversions

# Reading is slow, so save it as R object once ready

# If update fields is provided, then look for previously preprocessed file

load_initial_datafile <- function(datafile, ignore.fields, reload.data = FALSE) { 

  if (reload.data) {

    message("Reading reloaded data. This may take a while ...")
    # Read the raw data
    df.orig <- read_bibliographic_metadata(datafile,
      ignore.fields = ignore.fields,
      verbose = TRUE)

    # Save the raw data
    message("Saving data as Rds...")
    saveRDS(df.orig, file = "df.raw.Rds", compress = "xz")
    message("Data saved!")

  } else {

    message("Reading data previously saved as Rds.")
    df.orig <- readRDS("df.raw.Rds")

  }

  return (df.orig)

}


get_preprocessing_data <- function(df.orig, update.fields, ignore.fields) {

  message("Getting data for preprocessing.")

  if ("df0.Rds" %in% dir()) {
    message("Previously preprocessed data found in file df0.Rds.")
  }

  if (exists("update.fields") && !is.null(update.fields) && ("df0.Rds" %in% dir())) {

    message("Updating selected fields in previously saved data.")
    df.preprocessed <- readRDS("df.Rds")    
    conversions <- readRDS("conversions.Rds")
    
  } else {

    df.preprocessed <- NULL # as data is to be completely preprocessed
    conversions <- list()
    update.fields <- names(df.orig) # Update all fields
    message("Updating all fields: ")
    message(paste(update.fields, collapse = "; "))
    m <- setdiff(update.fields, c("system_control_number", "control_number", "original_row", "language", "title", "publisher", "subject_topic", "publication_topic", "author_name", "author_date", "publication_time", "physical_extent", "physical_dimension", "publication_place", "publication_geography", "title_uniform", "title_uniform2"))
    if (length(m) > 0) {
      warning(paste("Updates not defined for the following input fields:", paste(m , sep = ";")))
    }
  }

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

# df.orig <- load_initial_datafile(fs, ignore.fields, reload.data = FALSE)

# print("Frist 10 row of df.orig :")
# print(df.orig[1:10, ])

# preprocessing.data <- get_preprocessing_data(df.orig, update.fields, ignore.fields)
# # print(preprocessing.data)
