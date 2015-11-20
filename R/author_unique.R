#' @title author_unique
#' @description Form unique author identifiers by combining name and life years
#' @param df data.frame with fields "author_name", "author_birth", "author_death"
#' @return Character vector of unique author IDs
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica") 
#' @examples \dontrun{a <- author_unique(df)}
#' @keywords utilities
author_unique <- function (df) {
  author_name <- author_birth <- author_death <- NULL
  author_unique <- rep(NA, nrow(df))
  author_unique <- apply(df[, c("author_name", "author_birth", "author_death")], 1, function (x) {paste(x[[1]], " (", x[[2]], "-", x[[3]], ")", sep = "")})
  author_unique[is.na(df$author_name)] <- NA
  author_unique <- gsub(" \\(NA-NA\\)", "", author_unique)
  author_unique
}

