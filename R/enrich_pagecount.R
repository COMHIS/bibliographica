#' @title Enrich Page Count
#' @description Augment missing pagecounts based on mean estimates from available data.
#' @param x Preprocessed data.frame
#' @param estimate Which estimate to use for pagecount ("median.pages" / "mean.pages")
#' @return Augmented data.frame
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df2 <- enrich_pagecount(x)}
#' @keywords utilities
enrich_pagecount <- function(x, estimate = "median.pages")  {

  df <- x
  
  #if (mean(df$system_control_number.x == df$system_control_number.y, na.rm = T) == 1) {
  #  df$system_control_number <- df$system_control_number.x
  #  df$system_control_number.x <- NULL
  #  df$system_control_number.y <- NULL        
  #}

  message("Add volume info where missing")

  # If volcount field does not exist, then assume volcount 1 for
  # all documents. This is for compatibility reasons.
  # TODO: later fix downstream functions so that they can manage
  # without such artificially added volcounts
  if (!"volcount" %in% names(df)) {
    df$volcount <- rep(1, nrow(df))
  }

  # Same for volnumber
  if (!"volnumber" %in% names(df)) {
    df$volnumber <- rep(NA, nrow(df))
  }

  # --------------------------------------------------------------------------

  message("Estimate total pages for the docs where it is missing")
  df$pagecount.orig <- df$pagecount

  # Recognize categories
  df$singlevol <- is.singlevol(df)

  df$multivol  <- is.multivol(df)
  # For multivolume pagecount estimation
  # only include docs with <=10 volumes since
  # docs with more volumes are likely not
  # following average volume-wise page counts
  df$multivol[df$volcount >= 10] <- FALSE

  df$issue     <- is.issue(df)

  # --------------------------------------------------------------------------

  # Set volcount = 1 for all documents that were classified as single vol
  # This is necessary for later pagecount average calculations
  df$volcount[df$singlevol] <- 1

  # --------------------------------------------------------------------------

  message("Calculate average page counts based on available data")
  mean.pagecounts <- get_mean_pagecounts(df, exclude.plates = TRUE)
  
  message("..write into file")
  write.table(mean.pagecounts$singlevol,
              file = "mean_pagecounts_singlevol.csv",
              sep = ",",
              quote = F,
              row.names = F)
  write.table(mean.pagecounts$multivol,
              file = "mean_pagecounts_multivol.csv",
              sep = ",",
              quote = F,
              row.names = F)
  write.table(mean.pagecounts$issue,
              file = "mean_pagecounts_issue.csv",
              sep = ",",
              quote = F,
              row.names = F)

  # --------------------------------------------------------------------------

  message("Identify issues with missing pagecount and add page count estimates")
  # Issues with no pagecount, or pagecount only consisting of plates
  inds1 <- df$issue &
  	   (is.na(df$pagecount) | 
	      df$pagecount == df$pagecount.plate) 

#  if (length(which(inds1)) > 0) {
		df[inds1, "pagecount"] <- estimate_pages_issue(df[inds1,],
  	    		      mean.pagecounts$issue, estimate)

  df[inds1, "pagecount_from"] <- paste("estimate_issue", estimate, sep = "_")
#	}
  message("Multi-vol docs..")
  # .. and then take only those without page count
  # ... also consider docs with <10 pages having missing page info as
  # these are typically ones with only some plate page information and
  # missing real page information
  # .. and finally also enrich those where pagecount only consists of plates
  inds <- df$multivol & (is.na(df$pagecount) | (df$pagecount == df$pagecount.plate))
  inds2 <- inds
#	if (length(which(inds2)) > 0) {
		df[inds, "pagecount"] <- estimate_pages_multivol(df[inds,], mean.pagecounts$multivol, estimate)
		df[inds, "pagecount_from"] <- paste("estimate_multivolume", estimate, sep = "_")
#	}
  message("Single-vol docs missing pagecount..")
  inds3 <- df$singlevol &
  	   (is.na(df$pagecount) | 
	      df$pagecount == df$pagecount.plate) 

  df[inds3, "pagecount"] <- estimate_pages_singlevol(df[inds3,],
                                                     mean.pagecounts$singlevol, estimate)
  df[inds3, "pagecount_from"] <- paste("estimate_singlevolume", estimate, sep = "_")

  # Store information on cases where pages were estimated
  estimated.pagecount <- cbind(id = df$original_row,
  		       	       issue = inds1,
			       multivol = inds2,
			       singlevol = inds3)


  return (df)
}
