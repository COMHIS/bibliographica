#' @title Issue Page Estimation
#' @description Estimate pages for issues.
#' @param df data.frame of documents x variables
#' @param mean.pagecounts Page count estimates to be used
#' @param estimate Which estimate to use for pagecount ("median.pages" / "mean.pages")
#' @return Page count estimates
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{p <- estimate_pages_issue(df)}
#' @keywords utilities
estimate_pages_issue <- function (df, mean.pagecounts, estimate = "median.pages") {

  # Pick volume and gathering info
  v <- df$volcount
  g <- df$gatherings

  # Fill in missing volume info
  # Issues with volume number constitute one item from multi-volume series
  # so use 1 as volume count for these where missing
  v[which(is.na(v) & !is.na(df$volnumber))] <- 1

  # print("Pick the estimated page counts per vol separately for each doc size")
  pages.per.vol <- mean.pagecounts[match(g, mean.pagecounts$doc.dimension), estimate]
  
  # Multiply pages by volcount to get the total pagecount estimate
  page.estimate <- v * pages.per.vol

  # Add pagecount from plates (not included in the mean pagecounts)
  # assume that the plate info has been provided for the complete document
  # (not per volume)
  page.estimate <- page.estimate + df$pagecount.plate

  # Round to the closest integer
  page.estimate <- round(page.estimate)

  page.estimate
  
}