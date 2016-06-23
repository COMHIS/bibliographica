#' @title Mean Page Counts
#' @description Estimate page counts.
#' @param df data frame
#' @return Average page count.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # m <- mean_pagecounts(df)
#' @keywords utilities
mean_pagecounts <- function (df) {

  items <- volnumber <- parts <- volcount <- gatherings <- pagecount <- NULL

  pagecounts <- df %>% 
	     group_by(gatherings) %>% 
	     summarize(
	  mean.pages.per.vol = mean(pagecount/volcount, na.rm = T), 
	  median.pages.per.vol = median(pagecount/volcount, na.rm = T), 
	  n = n())

  colnames(pagecounts) <- c("doc.dimension", "mean.pages", "median.pages", "n")

  pagecounts


}

