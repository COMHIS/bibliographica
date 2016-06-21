#' @title Mean Pagecounts (Single Volume)
#' @description Calculate mean page counts for single volume documents.
#' @param df data frame
#' @return Average page count information
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # m <- mean_pagecounts_univol(df)
#' @keywords utilities
mean_pagecounts_univol <- function (df) {

  items <- volnumber <- parts <- volcount <- gatherings <- pagecount <- NULL
  if (!"univol" %in% names(df))  {df$univol = is.singlevol(df)}    
  if (!"volcount" %in% names(df))  {df$volcount <- rep(1, nrow(df))}
  if (!"volnumber" %in% names(df)) {df$volnumber <- rep(NA, nrow(df))}  

  # Ensure that
  # Include only docs that have a single volume in one part
  # (multi-volume books tend to have more pages)
  pagecounts <- dplyr::filter(df, univol) %>% 
		group_by(gatherings) %>% 
		summarize(
	mean.pages.per.vol = mean(na.omit(pagecount)), 
 	median.pages.per.vol = median(na.omit(pagecount)), 
		n = n())
  colnames(pagecounts) <- c("doc.dimension", "mean.pages.singlevol", "median.pages.singlevol", "n.singlevol")

  pagecounts

}

