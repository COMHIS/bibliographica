##
## CALCULATE AVERAGE DOC SIZES FROM THE ORIGINAL ENTRIES
# global variables used:
# df.preprocessed, update.fields
# TODO All functions should go under R/ folder

enrich_preprocessed_data <- function(data.validated, df.orig) {

  df.preprocessed <- data.validated$df.preprocessed
  update.fields   <- data.validated$update.fields
  conversions     <- data.validated$conversions

  check <- "enrich"

  if (any(c("publication_place", "publication_geography") %in% update.fields)) {
    source(system.file("extdata/enrich_geo.R", package = "bibliographica"))
    df.preprocessed <- enrich_geo(df.preprocessed)
  }

  # Always do. New field "author" needed for first edition recognition.
  # This is fast.
  if (any(c("author_name", "author_date") %in% update.fields)) {
    source(system.file("extdata/enrich_author.R", package = "bibliographica"))
    # this seems to take long even with only 100 entries in df.preprocessed? -vv 
    df.preprocessed <- enrich_author(df.preprocessed)
  }

  # Always do. This is fast.
  # Must be done after enrich_author
  #if ("publication_time" %in% update.fields) {
  source(system.file("extdata/enrich_years.R", package = "bibliographica"))
  df.preprocessed <- enrich_years(df.preprocessed, df.orig)
  #}


  if ("publisher" %in% update.fields) {

    message("Self-published docs where author is known but publisher not")
    # Note: also unknown authors are considered as self-publishers
    print("Add a separate self-published field")
    tmp <- self_published(df.preprocessed)
    df.preprocessed$self_published <- tmp$self_published
    df.preprocessed$publisher <- tmp$publisher

  }

  # -------------------------------------------------------------------

  if (any(c("physical_extent", "physical_dimension") %in% update.fields)) {

    source(system.file("extdata/enrich_dimensions.R", package = "bibliographica"))
    df.preprocessed <- enrich_dimensions(df.preprocessed)

    source(system.file("extdata/enrich_pagecount.R", package = "bibliographica"))
    df.preprocessed <- enrich_pagecount(df.preprocessed)

    print("Add estimated paper consumption")
    # Estimated print run size for paper consumption estimates
    printrun <- 1000 # This could be improved - varies in time !
    
    # Paper consumption in sheets
    # (divide document area by standard sheet area
    sheet.area <- subset(sheet_sizes(), format == "sheet")$area
    df.preprocessed <- mutate(df.preprocessed,
            paper = printrun * (pagecount/2) * (width * height)/sheet.area)

  }

  if (any(c("title", "subject_topic", "publication_topic") %in% update.fields)) {
    # Nothing defined yet
    NULL
  }

  if ("language" %in% update.fields) {
    NULL
  }

  print("Enrich OK")

  # setwd(old.wd)

  enriched.data <- list(df.preprocessed = df.preprocessed,
                        update.fields = update.fields,
                        conversions = conversions) 

  return (enriched.data)
}
