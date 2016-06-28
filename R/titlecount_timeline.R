#' @title Title Count Timeline
#' @description Compare title (document) count among selected groups.
#' @param x data frame
#' @param group_by Field indicating the groups to compare
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
titlecount_timeline <- function (x, group_by, nmin = 0, mode = "n") {

  publication_decade <- NULL

  x$group_by <- x[[group_by]]
  
  df2 <- x %>% filter(!is.na(group_by)) %>% group_by(publication_decade, group_by) %>%
     summarize(n = n())

  # Remove entries with too few occurrences
  df2 <- df2 %>% filter(!is.na(publication_decade) &
    group_by %in% setdiff(names(which(table(df2$group_by) >= nmin)), "NA"))
  #df2 <- df2 %>% dplyr::filter(group_by %in% setdiff(names(which(table(df2$group_by) >= nmin)), "NA"))  
  df2$group_by <- droplevels(df2$group_by)
  

  # Add percentages
  df3 = spread(df2, "publication_decade", "n", fill = 0)
  df3[, -1] = 100 * apply(df3[, -1], 2, function (x) {x/sum(x)})
  df3 = gather(df3, "group_by")
  colnames(df3) = c("group_by", "publication_decade", "percentage")
  df3$publication_decade = as.numeric(df3$publication_decade)

  # Combine counts and percentages
  dfs = dplyr::full_join(df2, df3)
  dfs$mode = dfs[[mode]]

  p <- ggplot(dfs, aes(y = mode, x = publication_decade, shape = group_by, linetype = group_by)) +
     geom_point(size = 4) +
     geom_line(aes(color = group_by), size = 1) +          
     ggtitle(paste("Title count in time by ", group_by)) +
     xlab("Year") + ylab(paste("Frequency (", mode, ")", sep = "")) +
     guides(linetype = guide_legend(keywidth = 5), shape = guide_legend(keywidth = 5)) +
     ggtitle("Title count timeline") 
     #scale_y_log10()
   list(plot = p, table = dfs)
}