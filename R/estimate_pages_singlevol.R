#' @title Page Estimation (Single Volume)
#' @description Estimate pages for single volume documents.
#' @param df data.frame of documents x variables
#' @param mean.pagecounts Page count estimates to be used
#' @return Page count estimates
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{p <- estimate_pages_singlevol(df)}
#' @keywords utilities
estimate_pages_singlevol <- function (df, mean.pagecounts) {

  g <- as.character(df$gatherings)
  inds <- match(g, as.character(mean.pagecounts$doc.dimension))
  page.estimates <- mean.pagecounts[inds, ]$median.pages

  # Round to the closest integer
  page.estimates <- round(page.estimates)

  page.estimates
    		
}