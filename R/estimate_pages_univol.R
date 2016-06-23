#' @title Page Estimation (Single Volume)
#' @description Estimate pages for single volume documents.
#' @param df data.frame of documents x variables
#' @param mean.pagecounts.singlevol Page count estimates to be used
#' @return Page count estimates
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{p <- estimate_pages_singlevol(df)}
#' @keywords utilities
estimate_pages_singlevol <- function (df, mean.pagecounts.singlevol) {

  g <- as.character(df$gatherings)
  inds <- match(g, as.character(mean.pagecounts.singlevol$doc.dimension))
  page.estimates <- mean.pagecounts.singlevol[inds, ]$median.pages.singlevol

  page.estimates
    		
}