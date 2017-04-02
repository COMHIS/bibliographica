#' @title Timeline Plot
#' @description Plot timeline for selected variable (possibly across various groups).
#' @param data data.frame for plotting
#' @param x time variable. Numeric field to summarize in the timeline. The number of entries (title count) per decade is shown by default.
#' @param y outcome variable
#' @param group Optional. Name for a data field that indicates groups to compare. If given, the different groups are indicated by lines.
#' @param nmin Include only entries with at least nmin absolute frequency
#' @param mode "absolute" or "relative"
#' @param time.window Time window for the timeline in years. Default: 10 (publication decade).
#' @return List:
#' \itemize{
#'   \item{plot}{ggplot object}
#'   \item{table}{summary table}
#' }
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{plot_timeline(df, "publication_year")}
#' @keywords utilities
plot_timeline <- function (data, x, y = "n", group = NULL, nmin = 0, mode = "absolute", time.window = 10) {

  if (y == "n") {
    y <- "titlecount"
  }

  publication_time <- dataid <- NULL

  if (y == "dataid") {
    stop("The dataid field is in internal use.")
  }

  dfx <- timeline(data, field = y, group = group, nmin = nmin, mode = mode, time.window = time.window, time.field = x)
  dfx$dataid <- rep("X", rep(nrow(dfx)))
  df <- dfx

  if (length(unique(df$group)) > 1) {
  
    p <- ggplot(df, aes(y = mode, x = publication_time,
       		       shape = group, linetype = group)) +
     geom_point(size = 4) +
     geom_line(aes(color = group), size = 1) +  
     guides(linetype = guide_legend(keywidth = 5),
     	    shape = guide_legend(keywidth = 5)) 

  } else {

    p <- ggplot(df, aes(y = mode, x = publication_time)) +
      	 	geom_bar(stat = "identity") 
    
  }

   p <- p + ylab(paste(y, " (", mode, ")", sep = "")) +
            xlab("Publication time") +
	    ggtitle(paste("Timeline for ", y, sep = ""))

   return(p)

}

