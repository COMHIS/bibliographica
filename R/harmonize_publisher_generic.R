#' @title Harmonize Publisher Generic
#' @description Main handler for publisher fields.
#' @param df.preprocessed Data frame assuming the place, year fields are already polished.
#' @param languages languages to consider in polishing
#' @return A vector with polished entries.
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
harmonize_publisher_generic <- function (df.preprocessed, languages = "english") {

  # TODO: consider unique entries only
  df <- df.preprocessed

  # Use corporate field for NA publishers
  if ("corporate" %in% names(df)) {
    inds <- which(is.na(df$publisher))
    df$publisher[inds] <- df$corporate[inds]
  }
  
  # TODO this might be overlapping with polish_publisher
  # the generic function which was called before the present function
  # We may want to skip this with Fennica, not sure
  df$publisher <- harmonize_publisher(df, languages = languages)

  # Convert S.N. into NA and Author into <Author>
  f <- system.file("extdata/NA_publishers.csv", package="bibliographica")
  synonymes <- read.csv(file = f, sep = "\t", fileEncoding = "UTF-8")
  harmonized_pubs <- map(df$publisher, synonymes, mode = "recursive")
  harmonized_pubs[harmonized_pubs == ""] <- NA

  # In fact only necessary to return mod
  return(harmonized_pubs)

}
