#' @title Harmonize Publisher Generic
#' @description Main handler for publisher fields.
#' @param df.orig Data frame with raw data, assuming the place, year fields are already polished.
#' @param languages languages to consider in polishing
#' @return A vector with polished entries.
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
harmonize_publisher_generic <- function (df.orig, languages = "english") {

  # TODO: consider unique entries only
  df$publisher[which(is.na(df$publisher))] <- df$corporate[which(is.na(df$publisher))]

  # TODO this might be overlapping with polish_publisher
  # the generic function which was called before the present function
  # We may want to skip this with Fennica, not sure
  combined_pubs <- harmonize_publisher(combined_pubs,
				       df.orig$publication_year,
				       languages = languages)

  # Convert S.N. into NA and Author into <Author>
  f <- system.file("extdata/NA_publishers.csv", package="bibliographica")
  synonymes <- read.csv(file = f, sep = "\t", fileEncoding = "UTF-8")
  harmonized_pubs <- map(combined_pubs$mod, synonymes, mode = "recursive")

  # In fact only necessary to return mod
  return(harmonized_pubs)

}
