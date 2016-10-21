#' @title Retrieve Relative Timeline 
#' @description Relative timeline for selected variable between two data sets.
#' @param x First data frame 
#' @param y Second data frame 
#' @param myfield Numeric field to summarize in the timeline (x/y). The number of entries (title count) per decade is used by default. If this argument is used, the sum of entries per decade for this field is given.
#' @param time.window Time window for the timeline in years. Default: 10 (publication decade).
#' @return data.frame
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{timeline_relative(df, "gatherings")}
#' @keywords utilities
timeline_relative <- function (x, y, myfield, time.window = 10) {

  publication_time <- X <- Y <- absolute <- relative <- group <- NULL
  nmin <- 0; mode <- "absolute"

  # Stats in the first data set
  tab0 <- timeline(x, field = myfield, nmin = nmin, mode = mode, time.window = time.window)
  tab0$group <- rep("X", nrow(tab0))

  # Stats in the second data set
  tab <- timeline(y, field = myfield, nmin = nmin, mode = mode, time.window = time.window)
  tab$group <- rep("Y", nrow(tab))

  # Limit the analysis on the same time window
  tab <- subset(tab, publication_time >= min(tab0$publication_time) &
                     publication_time <= max(tab0$publication_time))

  df <- bind_rows(tab, tab0)
  df$group <- factor(df$group)
  df <- df %>% select(publication_time, group, absolute) %>% 
               spread(key = "group", value = "absolute", fill = 0)
	     
  df <- df %>% mutate(fraction = 100 * X/Y)

  df

}
