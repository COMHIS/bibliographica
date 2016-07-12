#' @title Title Count Timeline
#' @description Compare title (document) count among selected groups.
#' @param x data frame
#' @param group Field indicating the groups to compare
#' @param nmin Include only entries with at least nmin occurrences
#' @param mode "n" (number of entries) or "percentage" (fraction of entries)
#' @return List:
#' \itemize{
#'   \item{plot}{ggplot object}
#'   \item{table}{summary table}
#' }
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{titlecount_timeline(df, "gatherings")}
#' @keywords utilities
titlecount_timeline <- function (x, group, nmin = 0, mode = "n") {

  publication_decade <- NULL
  x <- x[, c("publication_decade", group)]

  x$group <- x[[group]]

  df2 <- x %>% filter(!is.na(group)) %>%
               group_by(publication_decade, group) %>%
     	       summarise(n = n())

  # Remove entries with too few occurrences
  df2 <- df2 %>% filter(!is.na(publication_decade) &
    group %in% setdiff(unique(as.character(unname(unlist(df2[which(df2$n >= nmin), "group"])))), "NA"))
  df2$group <- factor(df2$group)
  df2$group <- droplevels(df2$group)

  # Add percentages
  df3 = spread(df2, "publication_decade", "n", fill = 0)
  df3[, -1] = 100 * apply(df3[, -1], 2, function (x) {x/sum(x)})
  #df3 = gather(df3, group)
  df3 = melt(df3, "group")  
  colnames(df3) = c("group", "publication_decade", "percentage")
  df3$publication_decade = as.numeric(as.character(df3$publication_decade))
  df3 <- df3[, c("publication_decade", "group", "percentage")]
  # Combine counts and percentages
  dfs = dplyr::full_join(df2, df3)
  dfs$mode = dfs[[mode]]

  if (length(unique(dfs$group))>1) {
    p <- ggplot(dfs, aes(y = mode, x = publication_decade,
       		       shape = group, linetype = group)) +
     geom_point(size = 4) +
     geom_line(aes(color = group), size = 1) +          
     ggtitle(paste("Title count in time by ", group)) +
     xlab("Year") + ylab(paste("Frequency (", mode, ")", sep = "")) +
     guides(linetype = guide_legend(keywidth = 5), shape = guide_legend(keywidth = 5)) +
     ggtitle("Title count timeline") 
  } else {
    p <- ggplot() + ggtitle("Insufficient data")
  }
  
   list(plot = p, table = dfs)
}