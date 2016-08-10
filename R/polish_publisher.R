#' @title Generic Cleanup for the Publisher Field
#' @description Main handler for publisher fields.
#' @param df Corresponding data frame assuming the place, year fields are already polished and available.
#' @param languages languages to consider in polishing
#' @return A vector with polished entries.
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
polish_publisher <- function (df, languages = "english") {

  df$publisher <- as.character(df$publisher)

  message("Use corporate field for NA publishers")
  if ("corporate" %in% names(df)) {
    message("Augmenting missing publisher entries with the corporate field")
    inds <- which(is.na(df$publisher))
    df$publisher[inds] <- as.character(df$corporate[inds])
  }

  message("Harmonize entries")
  pub <- harmonize_publisher(df, languages = languages)$mod

  message("Custom synonyme lists")
  # Convert S.N. into NA and Author into <Author>
  f <- system.file("extdata/NA_publishers.csv", package = "bibliographica")
  synonymes <- read.csv(file = f, sep = "\t", fileEncoding = "UTF-8")
  pub <- map(pub, synonymes, mode = "recursive")
  pub[pub == ""] <- NA

  message("Publisher polished.")
  return(pub)

}
