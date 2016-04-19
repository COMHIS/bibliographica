#' @title Identify issues
#' @description Identify documents that can be considered issues; based on other document info.
#' @details This function is used only to estimate pagecounts for documents with missing page count information.
#' Therefore no page count is considered in assessing the issue status.
#' @param df data.frame of documents x variables
#' @return Logical vector indicating the issues
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{issue <- is.issue(df)}
#' @keywords utilities
is.issue <- function (df) {

  # Gatherings 1to-4to (any number of vols) and missing page count
  selected.gatherings <- c("1to", "2small", "2to", "2long", "4small", "4to", "4long")
  inds1 <- df$gatherings %in% selected.gatherings & is.na(df$pagecount)
  
  # Docs of any size with >10 vols and missing page count
  inds2 <- df$volcount > 10 & is.na(df$pagecount)

  # If document size is among selected gatherings
  # or has many volumes
  # and does not have page info given
  # then consider it as an issue
  inds <- inds1 | inds2
  
  inds[is.na(inds)] <- FALSE

  inds
  
}
