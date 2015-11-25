#' @title author_unique
#' @description Form unique author identifiers by combining name and life years
#' @param df data.frame with fields "author_name", "author_birth", "author_death"
#' @param format name format ("last, first" or "first last")
#' @param initialize.first Convert first names into initials. Useful for removing duplicates when the name list contains different versions with both full name and the initials. Use of the life year fields helps to avoid mixing identical abbreviated names.
#' @return Character vector of unique author IDs
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica") 
#' @examples \dontrun{a <- author_unique(df)}
#' @export
#' @keywords utilities
author_unique <- function (df, format = "last, first", initialize.first = FALSE) {

  author_name <- author_birth <- author_death <- NULL

  author_unique <- rep(NA, nrow(df))

  first <- pick_firstname(df$author_name, format = format)
  last <- pick_lastname(df$author_name, format = format)  
  birth <- df$author_birth
  death <- df$author_death

  # Cut the full first names into initials
  if (initialize.first) {
    first <- name_initials(first)
  }

  author_unique <- apply(cbind(last, first, birth, death), 1, function (x) {paste(x[[1]], ", ", x[[2]], " (", x[[3]], "-", x[[4]], ")", sep = "")})  
  author_unique[is.na(df$author_name)] <- NA
  author_unique <- gsub(" \\(NA-NA\\)", "", author_unique)
  author_unique <- gsub("NA \\(NA-NA\\)", NA, author_unique)
  
  as.factor(unname(author_unique))

}

