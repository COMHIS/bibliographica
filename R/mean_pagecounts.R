#' @title Mean Page Counts
#' @description Estimate mean and median page counts.
#' @param df data frame, including the required fields.
#' @param pagecount.field Field to be used in estimation.
#' @param rounding Round to the closest integer
#' @return Average page count.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{m <- mean_pagecounts(df)}
#' @keywords utilities
mean_pagecounts <- function (df, pagecount.field = "pagecount", rounding = FALSE) {

  df$pgc <- df[[pagecount.field]]		

  # Do not accept zero pagecounts. Mark them as NA.
  df$pgc[df$pgc == 0] <- NA

  items <- volnumber <- parts <- volcount <- gatherings <- pgc <- NULL

  pagecounts <- df %>% 
	     group_by(gatherings) %>% 
	     summarize(
	    mean.pages.per.vol =   mean(pgc/volcount, na.rm = T), 
	  median.pages.per.vol = median(pgc/volcount, na.rm = T), 
	  n = n())

  colnames(pagecounts) <- c("doc.dimension", "mean.pages", "median.pages", "n")

  # Round to the closest integer
  if (rounding) {
    pagecounts$mean.pages <- round(pagecounts$mean.pages)
    pagecounts$median.pages <- round(pagecounts$median.pages)    
  }

  # Order gatherings factor levels
  pagecounts$doc.dimension <- factor(pagecounts$doc.dimension)
  pagecounts$doc.dimension <- order_gatherings(pagecounts$doc.dimension)

  pagecounts


}

