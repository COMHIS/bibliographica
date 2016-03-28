# Load packages
library(devtools)
library(dplyr)
library(bibliographica)
library(sorvi)
library(estc)

# Create the output directory if not yet exists
dir.create(output.folder)

# List fields to ignore
if (!exists(ignore.fields)) {ignore.fields <- NULL} 

# Read the raw data
df.orig <- read_bibliographic_metadata(fs, verbose = TRUE)

# Keep selected fields 
df.orig <- df.orig[, setdiff(names(df.orig), ignore.fields)]

# Save the raw data
# saveRDS(df.orig, file = "df.raw.RData", compress = "xz")




