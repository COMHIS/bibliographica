#' @title Mean Page Counts for All Document Types
#' @description Calculate mean page counts for different document types.
#' @param df data frame, including the required fields.
#' @param exclude.plates Exclude plate info from page counts.
#' @return Average page count information.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{m <- get_mean_pagecounts(df)}
#' @keywords utilities
get_mean_pagecounts <- function(df, exclude.plates = TRUE) {

  # Clean up		    
  gc()		    

  # Avoid warnings
  singlevol <- multivol <- issue <- pagecount <- NULL

  # Make sure that all pagecount estimates are based on the original data only
  # Calculate pagecounts without plates ! The plate info will be added afterwards
  # since this is often (only) available for docs that are missing page information
  # For gatherings, let us the final gatherings info since some documents are
  # reclassified (e.g. 1to -> 2fo) and this should be taken into account.
  dfs <- df
  dfs$pagecount <- dfs$pagecount.orig  
  if (exclude.plates) {
    dfs$pagecount <- dfs$pagecount - dfs$pagecount.plate
  }

  message("get_mean_pagecounts single volume")
  # For single-volume documents, use only the ones with >=32 pages
  # to estimate average page counts
  dfs.single <- dplyr::filter(dfs, singlevol & pagecount >= 32)
  mean.pagecounts.singlevol <- mean_pagecounts(dfs.single, rounding = TRUE) 

  message("get_mean_pagecounts multivolume")
  mean.pagecounts.multivol <- mean_pagecounts(dplyr::filter(dfs, multivol), rounding = TRUE)

  message("get_mean_pagecounts issue")
  mean.pagecounts.issue <- mean_pagecounts(dplyr::filter(dfs, issue), rounding = TRUE) 

  #message("get_mean_pagecounts join")
  #mean.pagecounts <- full_join(mean.pagecounts.singlevol,
  #		               mean.pagecounts.multivol,
  #			       by = "doc.dimension")
  #			       
  #mean.pagecounts <- full_join(mean.pagecounts,
  # 		               mean.pagecounts.issue,
  #			       by = "doc.dimension")
  #
 
  message("get_mean_pagecounts arrange into list")  
  mean.pagecounts.results <- list( multivol = mean.pagecounts.multivol,
                                  singlevol = mean.pagecounts.singlevol,
                                      issue = mean.pagecounts.issue)

  return(mean.pagecounts.results)

}
