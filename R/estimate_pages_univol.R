#' @title Page estimation for univols
#' @description Estimate pages for univols.
#' @param df data.frame of documents x variables
#' @param mean.pagecounts.univol Page count estimates to be used
#' @return Page count estimates
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{p <- estimate_pages_univol(df)}
#' @keywords utilities
estimate_pages_univol <- function (df, mean.pagecounts.univol) {

  g <- as.character(df$gatherings)
  inds <- match(g, as.character(mean.pagecounts.univol$doc.dimension))
  page.estimates <- mean.pagecounts.univol[inds, ]$median.pages.singlevol

  page.estimates
    		
}