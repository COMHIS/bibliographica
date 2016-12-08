#' @title Issue Page Estimation
#' @description Estimate pages for issues.
#' @param df data.frame of documents x variables
#' @param mean.pagecounts.issue Page count estimates to be used
#' @return Page count estimates
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{p <- estimate_pages_issue(df)}
#' @keywords utilities
estimate_pages_issue <- function (df, mean.pagecounts.issue) {

  # Pick volume and gathering info
  v <- df$volcount
  g <- df$gatherings

  # Fill in missing volume info
  # Issues with volume number constitute one item from multi-volume series
  # so use 1 as volume count for these where missing
  v[which(is.na(v) & !is.na(df$volnumber))] <- 1

  # print("Pick the estimated page counts per vol separately for each doc size")
  pages.per.vol <- mean.pagecounts.issue[match(g, mean.pagecounts.issue$doc.dimension), ]$median.pages.issue
  page.estimate <- v * pages.per.vol

  page.estimate
  
}