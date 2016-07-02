#' @title Filter dissertations
#' @description Gets dissertations from a single catalog or all of them
#' @param df.combined Data frame of processed data from both catalogs
#' @param catalog_name Name of catalog inspect. Default "ANY"
#' @param town Publication place. Default "ANY"
#' @return Filtered data frame
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
filter_dissertations <- function(df.combined, catalog_name="ANY", town="ANY") {
  
  if (catalog_name != "ANY") {
    dissertations <- df.combined %>% filter(catalog == catalog_name)
    dissertations <- dissertations %>% filter(dissertation==TRUE)
  } else {
    dissertations <- df.combined %>% filter(dissertation==TRUE)
  }

  if (town != "ANY") {
    dissertations <- dissertations %>% filter(publication_place == town)
  }
  
  return (dissertations)
}