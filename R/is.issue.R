#' @title Issue Identification
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

  # Gatherings 1to-4to (any number of vols) 
  # selected.gatherings <- c("1to", "bs", "2small", "2fo", "2long", "4small", "4to", "4long")
  # inds1 <- (df$gatherings %in% selected.gatherings) 
  
  # All docs with >30 vols
  if ("volcount" %in% names(df)) {
    inds2 <- df$volcount > 30 
  } else {
    inds2 <- rep(FALSE, nrow(df))
  }

  # All documents that have (non-NA) publication frequency
  inds3 <- rep(FALSE, nrow(df))  
  if ("publication_frequency" %in% names(df)) {    
    inds3 <- !is.na(df$publication_frequency)
  }

  # TODO: how to consider publication_interval which is sometimes available?
  
  # The use of till - from did not work - 90% of these were multivolumes in ESTC
  # and not issues
  # All multivols (>=2) that have publication period of >= 3 years
  #inds4 <- (df$publication_year_till - df$publication_year_from) >= 3  
  #if ("volcount" %in% names(df)) {
  #  inds4 <- inds4 & (df$volcount >= 2) 
  #}

  # Large gatherings and docs with many volumes are considered issues
  inds <- inds2 | inds3

  inds[is.na(inds)] <- FALSE

  inds
  
}

