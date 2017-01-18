#' @title Page Estimation for Multivolume Documents
#' @description Estimate pages for multi-volume documents.
#' @param df data.frame of documents x variables
#' @param pagecount.estimates Page count estimates to be used
#' @param estimate Which estimate to use for pagecount ("median.pages" / "mean.pages")
#' @return Page count estimates
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{p <- estimate_pages_multivol(df)}
#' @keywords utilities
estimate_pages_multivol <- function (df, pagecount.estimates, estimate = "median.pages") {

  # Pick gatherings, volume, and page info for multi-vol docs with missing page info
  g <- df$gatherings
  v <- df$volcount
  p <- df$pagecount

  # Docs that have a volnumber are assumed to constitute one item from multi-volume series
  # if volcount is not given; therefore use volcount 1 for them in page calculations
  inds <- which(is.na(df$volcount) & !is.na(df$volnumber)) 
  v[inds] <- 1
 
  # print("Pick the estimated page counts per vol separately for each doc size")
  pages.per.vol <- unlist(pagecount.estimates[match(g, pagecount.estimates$doc.dimension), estimate], use.names = FALSE)

  # print("Add estimated total page counts for all docs")
  page.estimate <- v * pages.per.vol

  # Assuming that page counts <10 represent cover pages only
  # also add these to the estimated page count, multiplied by the number of volumes
  # exclude plates at this point as they will be added later
  inds2 <- which(p <= 10)
  page.estimate[inds2] <- page.estimate[inds2] + v[inds2] * p[inds2] - df$pagecount.plate[inds2]

  # Now add pagecount from plates (not included in the mean pagecounts)
  # assume that the plate info has been provided for the complete document
  # (not per volume)
  page.estimate <- page.estimate + df$pagecount.plate

  # Round to the closest integer
  page.estimate <- round(page.estimate)

  page.estimate
  
}