#' @title Harmonize Publishers per Language
#' @description Harmonize publishers per language.
#' @param q Character vector
#' @param languages languages
#' @return Harmonized terms
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- harmonize_publishers_per_language(q, languages)}
#' @keywords utilities
harmonize_publishers_per_language <- function (q, languages) {

  q <- as.character(q)

  # Used to the first thing to do in this function
  for (language in languages) {
    if (language == "swedish") {
      f <- system.file("extdata/sv_publisher.csv", package="bibliographica")
    } else if (language == "english") {
      f <- system.file("extdata/en_publisher.csv", package="bibliographica")
    } else if (language == "finnish") {
      f <- system.file("extdata/fi_publisher.csv", package="bibliographica")
    } else if (language == "latin") {
      f <- system.file("extdata/lat_publisher.csv", package="bibliographica")
    } else {
      warning(paste0("Unknown language in languages: ", language))
    }

    synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")

    q <- map(q, synonyms, mode = "recursive")

  }

  q

}