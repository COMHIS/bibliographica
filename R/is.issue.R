#' @title Identify issues
#' @description Identify documents that can be considered issues; based on other document info.
#' @details This function is used only to estimate pagecounts for documents with missing page count information.
#' Therefore no page count is considered in assessing the issue status.
#' @param df data.frame of documents x variables
#' @param na.pagecount Logical. Include only documents with missing page count.
#' @return Logical vector indicating the issues
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{issue <- is.issue(df)}
#' @keywords utilities
is.issue <- function (df, na.pagecount = TRUE) {

  # Gatherings 1to-4to (any number of vols) 
  selected.gatherings <- c("1to", "2small", "2to", "2long", "4small", "4to", "4long")
  inds1 <- (df$gatherings %in% selected.gatherings) 
  
  # Any gatherings with >10 vols
  inds2 <- df$volcount > 10 

  # Large gatherings and docs with many volumes are considered issues
  inds <- inds1 | inds2

  # Include only documents with missing page count
  if (na.pagecount) {
    inds <- inds & is.na(df$pagecount)
  }

  inds[is.na(inds)] <- FALSE

  inds
  
}