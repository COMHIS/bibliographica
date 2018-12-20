#' @title Validate Publication Time
#' @description Preprocessing validator.
#' @param data.preprocessed Preprocessed data.
#' @param min.year Minimum accepted year; all accepted by default
#' @param max.year Maximum accepted year; current year by default
#' @return Modified data.
#' @export
#' @author Ville Vaara and Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # \dontrun{validate_publication_time(data.preprocessed)}
#' @keywords utilities
validate_publication_time <- function(x, min.year = NULL, max.year = NULL) {

  min.year <- (-2000)
  if (is.null(min.year)) {
    min.year <- -Inf
  }
  if (is.null(max.year)) {
    max.year <- as.numeric(format(Sys.time(), "%Y")) # this.year
  }

  message("Fix publication years")
  df <- x  
  # Remove apparent errors: no future publications or publications before historical times
  df$publication_year_from[which(df$publication_year_from > max.year)] <- NA
  df$publication_year_from[which(df$publication_year_from < min.year)] <- NA
  df$publication_year_till[which(df$publication_year_till > max.year)] <- NA
  df$publication_year_till[which(df$publication_year_till < min.year)] <- NA

  df

}
