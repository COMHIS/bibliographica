#' @title Identify single volume docs
#' @description Identify documents that can be considered single-volume; based on other document info.
#' @details This function is used only to estimate pagecounts for documents with missing page count information.
#' Therefore no page count is considered in assessing the issue status.
#' @param df data.frame of documents x variables
#' @return Logical vector indicating the single-volume docs
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{issue <- is.singlevol(df)}
#' @keywords utilities
is.singlevol <- function (df) {

  # Default single-vol docs:
  # those with a single volcount, or missing volcount
  inds <- (df$volcount == 1 | is.na(df$volcount))

  # Only include docs without volnumber
  # (those with volnumber should not be single vol docs)
  inds <- inds & is.na(df$volnumber)

  # Only include docs that have size info
  inds <- inds & !is.na(df$gatherings) 

  # Exclude large sizes
  inds <- inds & !df$gatherings %in% c("1to", "2small", "2to", "2long")

  # Set NAs to FALSE
  inds[is.na(inds)] <- FALSE

  inds

}
