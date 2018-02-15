#' @title Filter towns
#' @description Gets publications printed in a single town from a single catalog or all of them
#' @param df.combined Data frame of processed data from both catalogs
#' @param catalog_name Name of catalog to inspect. Default "ANY"
#' @param town Publication place. Default "ANY"
#' @return Filtered data frame
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
filter_towns <- function(df.combined, catalog_name="ANY", town="ANY") {
  
  if (catalog_name != "ANY") {
    publications <- df.combined %>% filter(df.combined$catalog == catalog_name)
    publications <- publications %>% filter(publications$publication_place==town)
  } else {
    publications <- df.combined %>% filter(df.combined$publication_place==town)
  }
  
  return (publications)
}