#' @title Page estimation for multivols
#' @description Estimate pages for multivols.
#' @param df data.frame of documents x variables
#' @param mean.pagecounts.multivol Page count estimates to be used
#' @return Page count estimates
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{p <- estimate_pages_multivol(df)}
#' @keywords utilities
estimate_pages_multivol <- function (df, mean.pagecounts.multivol) {

  # Pick gatherings, volume, and page info for multi-vol docs with missing page info
  g <- df$gatherings
  v <- df$volcount
  p <- df$pagecount

  # Docs that have a volnumber are assumed to constitute one item from multi-volume series
  # if volcount is not given; therefore use volcount 1 for them in page calculations
  inds <- which(is.na(df$volcount) & !is.na(df$volnumber)) 
  v[inds] <- 1
 
  # print("Pick the estimated page counts per vol separately for each doc size")
  pages.per.vol <- mean.pagecounts.multivol[match(g, mean.pagecounts.multivol$doc.dimension), ]$median.pages.multivol

  # print("Add estimated total page counts for all docs")
  page.estimate <- v * pages.per.vol

  # Assuming that page counts <10 represent cover pages only
  # also add these to the estimated page count, multiplied by the number of volumes
  inds2 <- which(p <= 10)
  page.estimate[inds2] <- page.estimate[inds2] + v[inds2] * p[inds2]

  page.estimate
  
}