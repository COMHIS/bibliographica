#' @title Paper consumption timeline
#' @description Compare paper consumption among selected groups
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
#' @examples \dontrun{paper_timeline(df, "gatherings")}
#' @keywords utilities
paper_timeline <- function (x, field, nmin = 0) {

  paper.consumption.km2 <- publication_decade <- NULL

  x$field <- x[[field]]

  df2 <- x %>% group_by(publication_decade, field) %>%
     summarize(paper.consumption.km2 = sum(paper.consumption.km2, na.rm = TRUE), n = n())

  # Remove entries with too few occurrences
  df2 <- df2 %>% filter(any(field == setdiff(names(which(table(df2$field) >= nmin)), "NA")))
  df2$field <- droplevels(df2$field)

  p <- ggplot(df2, aes(y = paper.consumption.km2, x = publication_decade, shape = field, linetype = field)) +
     geom_point(size = 4) +
     #geom_smooth(method = "loess", size = 1, color = "black") +
     geom_line(size = 1, color = "black") +     
     ggtitle(paste("Paper consumption in time by ", field)) +
     xlab("Year") + ylab("Paper consumption (km2)") +
     guides(linetype = guide_legend(keywidth = 5), shape = guide_legend(keywidth = 5)) +
     ggtitle("Paper consumption")
   list(plot = p, table = df2)
}