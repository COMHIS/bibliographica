#' @title mean_pagecounts_issue
#' @description Estimate page counts for issues
#' @param df data frame
#' @return Average page count information
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' @examples # m <- mean_pagecounts_issue(df)
#' @keywords utilities
mean_pagecounts_issue <- function (df) {

  items <- volnumber <- parts <- volcount <- gatherings <- pagecount <- NULL

  pagecounts <- filter(df, 
  	     volcount == 1 & 
	     pagecount >= 8 & 
	     pagecount <= 50) %>% 
	     group_by(gatherings) %>% 
	     summarize(
	  mean.pages.per.vol = mean(pagecount/volcount, na.rm = T), 
	  median.pages.per.vol = median(pagecount/volcount, na.rm = T), 
	  n = n())

  colnames(pagecounts) <- c("doc.dimension", "mean.pages.issue", "median.pages.issue", "n.issue")

  pagecounts


}

