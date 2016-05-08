#' @title Title Count Timeline
#' @description Compare title (document) count among selected groups.
#' @param x data frame
#' @param field Field indicating the groups to compare
#' @param nmin Include only entries with at least nmin occurrences
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
titlecount_timeline <- function (x, field, nmin = 0) {

  publication_decade <- NULL

  x$field <- x[[field]]
  
  df2 <- x %>% filter(!is.na(field)) %>% group_by(publication_decade, field) %>%
     summarize(n = n())

  # Remove entries with too few occurrences
  df2 <- dplyr::filter(df2, field %in% setdiff(names(which(table(df2$field) >= nmin)), "NA"))
  df2$field <- droplevels(df2$field)

  p <- ggplot(df2, aes(y = n, x = publication_decade, shape = field, linetype = field)) +
     geom_point(size = 4) +
     geom_line(aes(color = field), size = 1) +          
     ggtitle(paste("Title count in time by ", field)) +
     xlab("Year") + ylab("Title count (n)") +
     guides(linetype = guide_legend(keywidth = 5), shape = guide_legend(keywidth = 5)) +
     ggtitle("Title count") 
     #scale_y_log10()
   list(plot = p, table = df2)
}