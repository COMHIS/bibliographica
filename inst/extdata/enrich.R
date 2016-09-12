##
## CALCULATE AVERAGE DOC SIZES FROM THE ORIGINAL ENTRIES
##

check <- "enrich"

if (any(c("publication_place", "publication_geography") %in% update.fields)) {
  source(system.file("extdata/enrich_geo.R", package = "bibliographica"))
}

# Always do. New field "author" needed for first edition recognition.
# This is fast.
if (any(c("author_name", "author_date") %in% update.fields)) {
  source(system.file("extdata/enrich_author.R", package = "bibliographica"))
}

# Always do. This is fast.
# Must be done after enrich_author
#if ("publication_time" %in% update.fields) {
  source(system.file("extdata/enrich_years.R", package = "bibliographica"))
#}


if ("publisher" %in% update.fields) {

  print("Self-published docs where author is known but publisher not")
  # Note: also unknown authors are considered as self-publishers
  print("Add a separate self-published field")
  tmp <- self_published(df.preprocessed)
  df.preprocessed$self_published <- tmp$self_published
  df.preprocessed$publisher <- tmp$publisher

}

# -------------------------------------------------------------------

if (any(c("physical_extent", "physical_dimension") %in% update.fields)) {

  source(system.file("extdata/enrich_dimensions.R", package = "bibliographica"))

  source(system.file("extdata/enrich_pagecount.R", package = "bibliographica"))

  print("Add estimated paper consumption")

  # Estimated print run size for paper consumption estimates
  printrun <- 1000 # This could be improved - varies in time !
  
  # Paper consumption in sheets
  # (divide document area by standard sheet area
  sheet.area <- subset(sheet_sizes(), format == "sheet")$area
  df.preprocessed <- mutate(df.preprocessed,
          paper = printrun * (pagecount/2) * (width * height)/sheet.area)

  df.preprocessed <- mutate(df.preprocessed, paper.check =
   		      width * height * pagecount/2 * (1/1e10) * printrun)

}

if (c("title", "subject_topic", "publication_topic") %in% update.fields) {
  # Nothing defined yet
  NULL
}

if ("language" %in% update.fields) {
 
  NULL
  
}

print("Enrich OK")

# ---------------------------------------

