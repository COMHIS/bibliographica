#' @title Get dissertations
#' @description Gets dissertations from a single catalog or all of them
#' @param processed_frame Data frame of processed data
#' @param catalog_name Name of catalog inspect. Default "ANY"
#' @param before Upper limit of publication year (self included)
#' @return Data frame of core fields
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
get_dissertations <- function(processed_frame, catalog_name="ANY", before=NULL) {
  
  if (catalog_name != "ANY") {
    dissertations <- which(processed_frame$catalog == catalog_name)
    dissertations <- intersect(dissertations, which(processed_frame$diss != ""))
  } else {
    dissertations <- which(processed_frame$diss != "")
  }
  
  if (!(is.null(before))) {
    dissertations <- intersect(dissertations, which(processed_frame$from <= before))
  }
  
  return (processed_frame[dissertations,])
}