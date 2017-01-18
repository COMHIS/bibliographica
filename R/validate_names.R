#' @title Validate Names
#' @description Validation for names.
#' @param df.preprocessed Preprocessed data.frame.
#' @return Modified data.frame
#' @export
#' @author Ville Vaara and Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # \dontrun{validate_names(df.preprocessed)}
#' @keywords utilities
validate_names <- function(df.preprocessed) {

  message("Validate author names. Set non-valid names to NA")

  # Some basic validation
  # "V. P" -> NA
  df.preprocessed$author_name[grep("^[A-Z|a-z][\\.|\\,]+ *[A-Z|a-z]$", df.preprocessed$author_name)] <- NA

  # "V. P. H" -> NA
  df.preprocessed$author_name[grep("^[A-Z|a-z][\\.|\\,]+ *[A-Z|a-z][\\.|\\,]+ *[A-Z|a-z][\\.|\\,]*$", df.preprocessed$author_name)] <- NA

  # "V P" -> NA
  df.preprocessed$author_name[grep("^[A-Z|a-z] +[A-Z|a-z]$", df.preprocessed$author_name)] <- NA

  # "V P H" -> NA
  df.preprocessed$author_name[grep("^[A-Z|a-z] +[A-Z|a-z] +[A-Z|a-z]$", df.preprocessed$author_name)] <- NA

  return (df.preprocessed)
}

# -----------------------------------------------------------------

# Author name validation with ready made lists is rather time-consuming
# Hence skip

#v <- validate_names(df.preprocessed$author_name, "full")
#discard.inds <- !v$valid
  
## Save discarded names for later analysis
#discarded.author.table <- rev(sort(table(as.character(df.preprocessed$author[discard.inds]))))
#discarded.author.firstnames <- v$invalid.first
#discarded.author.lastnames <- v$invalid.last  

## Remove discarded names from the list
# df.preprocessed$author_name[discard.inds] <- NA

# -----------------------------------------------------------------