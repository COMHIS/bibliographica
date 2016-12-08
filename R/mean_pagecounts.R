#' @title Mean Page Counts
#' @description Estimate page counts.
#' @param df data frame, including the required fields.
#' @param pagecount.field Field to be used in estimation.
#' @return Average page count.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{m <- mean_pagecounts(df)}
#' @keywords utilities
mean_pagecounts <- function (df, pagecount.field = "pagecount") {

  df$pgc <- df[[pagecount.field]]		

  items <- volnumber <- parts <- volcount <- gatherings <- pgc <- NULL

  pagecounts <- df %>% 
	     group_by(gatherings) %>% 
	     summarize(
	  mean.pages.per.vol = mean(pgc/volcount, na.rm = T), 
	  median.pages.per.vol = median(pgc/volcount, na.rm = T), 
	  n = n())

  colnames(pagecounts) <- c("doc.dimension", "mean.pages", "median.pages", "n")

  pagecounts


}

