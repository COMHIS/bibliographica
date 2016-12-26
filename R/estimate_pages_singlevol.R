#' @title Page Estimation (Single Volume)
#' @description Estimate pages for single volume documents.
#' @param df data.frame of documents x variables
#' @param pagecount.estimates Page count estimates to be used
#' @return Page count estimates
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{p <- estimate_pages_singlevol(df)}
#' @keywords utilities
estimate_pages_singlevol <- function (df, pagecount.estimates) {

  g <- as.character(df$gatherings)
  inds <- match(g, as.character(pagecount.estimates$doc.dimension))
  page.estimate <- pagecount.estimates[inds, ]$median.pages

  # Add pagecount from plates (not included in the mean pagecounts)
  # assume that the plate info has been provided for the complete document
  # (not per volume)
  page.estimate <- page.estimate + df$pagecount.plate
  
  # Round to the closest integer
  page.estimate <- round(page.estimate)

  page.estimate
    		
}