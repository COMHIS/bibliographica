#' @title Timeline Plot
#' @description Plot timeline for selected variable (possibly across various groups).
#' @param x First data.frame 
#' @param y Second data.frame. Optional, for comparison.
#' @param field Numeric field to summarize in the timeline. The number of entries (title count) per decade is shown by default, But if this argument is used, the sum of entries per decade for this field is given.
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
#' @details If two data.frames (x,y) are provided in the function arguments, a comparison plot will be generated.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{timeline(df, "gatherings")}
#' @keywords utilities
plot_timeline <- function (x, y = NULL, field = "titlecount", group = NULL, nmin = 0, mode = "absolute", time.window = 10) {

  if (field == "dataid") {
    stop("The dataid field is in internal use.")
  }
  
  dfx <- timeline(x, field = field, group = group, nmin = nmin, mode = mode, time.window = time.window)
  dfx$dataid <- rep("X", rep(nrow(dfx)))
  df <- dfx

  if (!is.null(y)) {
    dfy <- timeline(y, field = field, group = group, nmin = nmin, mode = mode, time.window = time.window)
    dfy$dataid <- rep("Y", rep(nrow(dfy)))
    df <- bind_rows(dfx, dfy)
  }

  if (length(unique(df$group)) > 1 && is.null(y)) {
    p <- ggplot(df, aes(y = mode, x = publication_time,
       		       shape = group, linetype = group)) +
     geom_point(size = 4) +
     geom_line(aes(color = group), size = 1) +               
     guides(linetype = guide_legend(keywidth = 5), shape = guide_legend(keywidth = 5)) 

  } else if (!is.null(y)) {

    # Mark NAs to 0
    df2 <- df %>% select(publication_time, mode, dataid) %>%
               spread(publication_time, mode, fill = 0) %>%
  	       gather(publication_time, mode, -dataid)

    p <- ggplot(df2, aes(y = mode, x = publication_time, fill = dataid)) +
      	   geom_bar(stat = "identity", position = "dodge", color = "black") +
	   scale_fill_manual(values = c("black", "darkgray")) +
	   theme(axis.text.x = element_text(angle = 90))

  } else {

    p <- ggplot(df, aes(y = mode, x = publication_time)) +
      	 	geom_bar(stat = "identity") 
    
  }

   p <- p + ylab(paste(field, " (", mode, ")", sep = "")) +
            xlab("Publication time") +
	    ggtitle(paste("Timeline for ", field, sep = ""))

   return(p)

}

