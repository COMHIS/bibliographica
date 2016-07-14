#' @title Compare Title Count and Paper Consumption
#' @description Compare title count and paper consumption for selected field.
#' @param x data frame
#' @param field Field to analyze
#' @param selected Limit the analysis on selected entries
#' @param plot.mode "text" or "point"
#' @return List:
#' \itemize{
#'   \item{plot}{ggplot object}
#'   \item{table}{summary table}
#' }
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{compare_title_paper(df, "author")}
#' @keywords utilities
compare_title_paper <- function (x, field, selected = NULL, plot.mode = "text") {

  paper <- publication_decade <- titles <- paper <- NULL

  x$field <- x[[field]]

  # Nothing selected - take all
  if (is.null(selected)) {selected <- unique(x$field)}

  x <- x %>% filter(!is.na(field)) %>%
            filter(field %in% selected) %>%
	    group_by(field) %>%	    
            summarize(titles = n(),
                      paper = sum(paper, na.rm = T))


  # Prepare the plot
  p <- ggplot(x, aes(x = titles, y = paper)) +
    xlab("Title count") + ylab("Standard sheets") + 
    ggtitle(paste("Title count versus paper (", field, ")")) +
    scale_x_log10() + scale_y_log10() 

  if (plot.mode == "text") {
    p <- p + geom_text(aes(label = field), size = 5) 
  } else if (plot.mode == "point") {
    p <- p + geom_point(size = 2)
  }

  colnames(x)[[1]] <- field

  list(plot = p, table = x)

}

