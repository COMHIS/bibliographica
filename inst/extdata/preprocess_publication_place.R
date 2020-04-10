# Preprocess publication_place field
field <- "publication_place"

# Polish place names
place <- polish_place(df.orig[[field]], remove.unknown = FALSE)

# Add country and geocoordinates
tmp <- enrich_geo(place)

# Add identifier that couples the original and preprocessed
# versions even after possible subsettings
df <- data.frame(original_row = df.orig$original_row,
                 publication_place = tmp$place,
                 publication_country = tmp$country)

# Store this data.frame
preprocessed[[field]] <- df

# Summarize the data and discarded entries
# This generates all summary tables that are linked at
# https://github.com/COMHIS/cerl/blob/master/publicationplace.md
tmp <- generate_summary_tables_geo(df, df.orig, output.folder)

# This can be modified at:
# bibliographica::inst/extdata/publicationplace.Rmd
# Sometimes we like to restricted the evaluated time period
# (see analysis.init.R) but for now, let's just do all
#sf <- generate_summaryfiles(df, df.orig, author = author, output.folder = output.folder, ntop = ntop, summaries = "publicationplace")

