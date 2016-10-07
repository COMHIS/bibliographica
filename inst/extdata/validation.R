check <- "validation"

validate_preprocessed_data <- function(data.preprocessed) {

  df.preprocessed <- data.preprocessed$df.preprocessed
  update.fields   <- data.preprocessed$update.fields
  conversions     <- data.preprocessed$conversions # isnt used in this function -vv

  min.year <- (-2000)
  max.year <- as.numeric(format(Sys.time(), "%Y")) # this.year


  if ("publication_time" %in% update.fields) {

    print("Fix publication years")
    # Remove apparent errors: no future publications or publications before historical times
    df.preprocessed$publication_year_from[which(df.preprocessed$publication_year_from > max.year)] <- NA
    df.preprocessed$publication_year_from[which(df.preprocessed$publication_year_from < min.year)] <- NA
    df.preprocessed$publication_year_till[which(df.preprocessed$publication_year_till > max.year)] <- NA
    df.preprocessed$publication_year_till[which(df.preprocessed$publication_year_till < min.year)] <- NA

  }


  if ("author_date" %in% update.fields) {

    # Author life years cannot exceed the present year
    # If they do, set to NA
    inds <- which(df.preprocessed$author_birth > max.year)
    if (length(inds) > 0) {
      df.preprocessed[inds, "author_birth"] <- NA
    }
    inds <- which(df.preprocessed$author_death > max.year)
    if (length(inds) > 0) {
       df.preprocessed[inds, "author_death"] <- NA
    }
    # Death must be after birth
    # If this is not the case, set the life years to NA
    inds <- which(df.preprocessed$author_death < df.preprocessed$author_birth)
    if (length(inds) > 0) {
      df.preprocessed[inds, "author_birth"] <- NA
      df.preprocessed[inds, "author_death"] <- NA
    }
  }

  # -----------------------------------------------------------------

  if ("author_name" %in% update.fields) {
    source(system.file("extdata/validate_names.R", package = "bibliographica"))
    df.preprocessed <- validate_names(df.preprocessed)
  }

  validated.data <- list(df.preprocessed = df.preprocessed,
                         update.fields = update.fields,
                         conversions = conversions) 

  return (validated.data)
}
