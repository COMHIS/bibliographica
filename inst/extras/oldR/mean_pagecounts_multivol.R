#' @title Mean Pagecounts (Multi-Volume)
#' @description Calculate mean page counts for multi-volume documents.
#' @param df data frame
#' @return Average page count information
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # m <- mean_pagecounts_multivol(df)
#' @keywords utilities
mean_pagecounts_multivol <- function (df) {

  items <- volnumber <- parts <- volcount <- gatherings <- pagecount <- NULL
  if (!"multivol" %in% names(df))  {df$multivol = is.multivol(df)}  
  if (!"volcount" %in% names(df))  {df$volcount <- rep(1, nrow(df))}
  if (!"volnumber" %in% names(df)) {df$volnumber <- rep(NA, nrow(df))}  

  # Include only those items that have multiple volumes
  #    (multi-volume books tend to have more pages)
  #    and calculate pages per volume
  #    we ignore the fact that in some cases volumes have multiple parts
  #    ie. parts may be different from volcount
  #    Also: "449 v., plates :" -> pagecount = 4; ignore these
  pagecounts <- filter(df, multivol) %>% 
		group_by(gatherings) %>% 
		summarize(
  mean.pages.per.vol = mean(na.omit(pagecount/volcount)),
  median.pages.per.vol = median(na.omit(pagecount/volcount)),
  n = n())
  colnames(pagecounts) <- c("doc.dimension", "mean.pages.multivol", "median.pages.multivol", "n.multivol")

  pagecounts

}


