check <- "validation"
# max.pagecount: pagecount will be marked NA for docs that exceed this limit

validate_preprocessed_data <- function(data.preprocessed, max.pagecount = 5000) {

  df.preprocessed <- data.preprocessed$df.preprocessed
  update.fields   <- data.preprocessed$update.fields
  conversions     <- data.preprocessed$conversions # isnt used in this function -vv

  # Consider all fields if update.fields is not specifically defined
  if (is.null(update.fields)) {
    update.fields <- names(df.preprocessed)
  }

  min.year <- (-2000)
  max.year <- as.numeric(format(Sys.time(), "%Y")) # this.year

  # Some documents have extremely high pagecounts
  # (up to hundreds of thousands of pages)
  # MT + LL selected 5000 pages as the default threshold.
  # If the document has more pages than this, the pagecount
  # info will be removed as unreliable
  # The ESTC seemed to have 4 documents (out of ~5e5) affected
  # with estimated pagecount over 5000
  # Also remove negative and zero pagecounts; should not be possible
  if ("physical_extent" %in% update.fields) {
    df.preprocessed$pagecount[df.preprocessed$pagecount > max.pagecount] <- NA
    df.preprocessed$pagecount[df.preprocessed$pagecount <= 0] <- NA
    # Round page counts to the closest integer if they are not already integers
    df.preprocessed$pagecount <- round(df.preprocessed$pagecount)
  }

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

    # Publication year must be after birth
    # FIXME: should we let these through to the final summaries
    # - this could help to spot problems ?
    inds <- which(df.preprocessed$author_birth > df.preprocessed$publication_year_from)
    if (length(inds) > 0) {
      df.preprocessed[inds, "author_birth"] <- NA
      df.preprocessed[inds, "author_death"] <- NA
      df.preprocessed[inds, "publication_year_from"] <- NA
      df.preprocessed[inds, "publication_year_till"] <- NA      
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
