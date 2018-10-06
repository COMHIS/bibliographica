#' @title Validate Preprocessed Data
#' @description Preprocessing validators and some adjustments.
#' @param data.preprocessed Preprocessed data.
#' @param max.pagecount Upper gap for the pagecount for ocs that exceed this limit.
#' @return Modified data.
#' @export
#' @author Ville Vaara and Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # \dontrun{validate_preprocessed_data(data.preprocessed)}
#' @keywords utilities
validate_preprocessed_data <- function(data.preprocessed, max.pagecount = 5000) {

  df <- data.preprocessed$df.preprocessed
  update.fields   <- data.preprocessed$update.fields
  conversions     <- data.preprocessed$conversions # isnt used in this function -vv

  # Consider all fields if update.fields is not specifically defined
  if (is.null(update.fields)) {
    update.fields <- names(df)
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

    # Apply gap on the highest pagecounts
    df$pagecount[df$pagecount > max.pagecount] <- max.pagecount
    df$pagecount[df$pagecount <= 0] <- NA
    # Round page counts to the closest integer if they are not already integers
    df$pagecount <- round(df$pagecount)
  }

  if ("publication_time" %in% update.fields) {

    message("Fix publication years")
    # Remove apparent errors: no future publications or publications before historical times
    df$publication_year_from[which(df$publication_year_from > max.year)] <- NA
    df$publication_year_from[which(df$publication_year_from < min.year)] <- NA
    df$publication_year_till[which(df$publication_year_till > max.year)] <- NA
    df$publication_year_till[which(df$publication_year_till < min.year)] <- NA

  }

  if ("author_date" %in% update.fields) {

    # Author life years cannot exceed the present year
    # If they do, set to NA
    inds <- which(df$author_birth > max.year)
    if (length(inds) > 0) {
      df[inds, "author_birth"] <- NA
    }
    inds <- which(df$author_death > max.year)
    if (length(inds) > 0) {
       df[inds, "author_death"] <- NA
    }
    
    # Death must be after birth
    # If this is not the case, set the life years to NA
    inds <- which(df$author_death < df$author_birth)
    if (length(inds) > 0) {
      df[inds, "author_birth"] <- NA
      df[inds, "author_death"] <- NA
    }

    # Author life - make sure this is in numeric format
    df$author_birth <- as.numeric(as.character(df$author_birth))
    df$author_death <- as.numeric(as.character(df$author_death))  

    # Publication year must be after birth
    # FIXME: should we let these through to the final summaries
    # - this could help to spot problems ?
    inds <- which(df$author_birth > df$publication_year_from)
    if (length(inds) > 0) {
      df[inds, "author_birth"] <- NA
      df[inds, "author_death"] <- NA
      df[inds, "publication_year_from"] <- NA
      df[inds, "publication_year_till"] <- NA      
    }

  }

  if ("author_name" %in% update.fields) {
    df <- validate_names(df)
  }

  validated.data <- list(df.preprocessed = df,
                         update.fields = update.fields,
                         conversions = conversions) 

  return (validated.data)
}
