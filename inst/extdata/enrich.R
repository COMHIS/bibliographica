##
## CALCULATE AVERAGE DOC SIZES FROM THE ORIGINAL ENTRIES
##

if (c("publication_place", "publication_geography") %in% update.fields) {
  source(system.file("extdata/enrich_geo.R", package = "bibliographica"))
}

if ("publication_time" %in% update.fields) {
  source(system.file("extdata/enrich_years.R", package = "bibliographica"))
}

if (c("author_name", "author_date") %in% update.fields) {
  source(system.file("extdata/enrich_author.R", package = "bibliographica"))
}

if ("publisher" %in% update.fields) {

  print("Self-published docs where author is known but publisher not")
  # Note: also unknown authors are considered as self-publishers
  print("Add a separate self-published field")
  tmp <- self_published(df.preprocessed)
  df.preprocessed$self_published <- tmp$self_published
  df.preprocessed$publisher <- tmp$publisher

}



if ("language" %in% update.fields) {
 
  # Enrich language
  dfl <- select(df.preprocessed, starts_with("language"))
  l <- capitalize(gsub("language\\.", "", names(dfl))); # Language names
  df.preprocessed$language <- factor(apply(dfl, 1, function (x) { paste(l[x], collapse = ";")  })) # List languages

}

# -------------------------------------------------------------------

if (c("physical_extent", "physical_dimension") %in% update.fields) {

  source(system.file("extdata/enrich_pagecount.R", package = "bibliographica"))

  source(system.file("extdata/enrich_dimensions.R", package = "bibliographica"))

  print("Add estimated paper consumption")
  # One m2 is 100 * 100 cm2 = 1e4 cm2
  # One km2 is 1000 * 1000 m2 = 1e6 m2 = 1e10 cm2
  # Estimated average print run per document: 1000
  printrun <- 1000 # This could be improved - varies in time !
  df.preprocessed <- mutate(df.preprocessed, paper.consumption.km2 = width * height * pagecount/2 * (1/1e10) * printrun)

}

if (c("title", "subject_topic", "publication_topic") %in% update.fields) {
  # Nothing defined yet
  NULL
}




