#' @title Create unique author ID
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

  dfa <- df[, c("author_name", "author_birth", "author_death")]
  dfa.orig <- dfa
  dfa.uniq <- unique(dfa.orig)
  # Entry IDs
  id.orig <- apply(dfa.orig, 1, function (x) {paste(as.character(x), collapse = "")})
  id.uniq <- apply(dfa.uniq, 1, function (x) {paste(as.character(x), collapse = "")})
  match.inds <- match(id.orig, id.uniq)
  rm(id.orig)  

  author_unique <- rep(NA, nrow(dfa.uniq))
  first <- pick_firstname(dfa.uniq$author_name, format = format, keep.single = FALSE)
  first[is.na(first) | first == "NA"] <- ""

  last  <- pick_lastname(dfa.uniq$author_name, format = format, keep.single = TRUE)

  author_unique <- apply(cbind(last, first, dfa.uniq$author_birth, dfa.uniq$author_death), 1, function (x) {paste(x[[1]], ", ", x[[2]], " (", x[[3]], "-", x[[4]], ")", sep = "")})  
  author_unique[is.na(dfa.uniq$author_name)] <- NA
  author_unique <- gsub(" \\(NA-NA\\)", "", author_unique)
  author_unique <- gsub("NA \\(NA-NA\\)", NA, author_unique)
  author_unique <- gsub("\\, NA$", "", author_unique)

  as.character(unname(author_unique))[match.inds]

}

