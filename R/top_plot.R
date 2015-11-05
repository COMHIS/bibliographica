#' @title top_plot
#' @description Plot the top entries for a given field in a data frame
#'
#' @param df Data frame, vector or factor
#' @param field Field to show
#' @param ntop Number of top entries to show
#' @param highlight Entries from the 'field' to be highlighted
#' @return ggplot object
#' @import ggplot2
#' @export
#' @importFrom dplyr count
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{p <- top_plot(df, field, 50)}
#' @keywords utilities
top_plot <- function (df, field = NULL, ntop = NULL, highlight = NULL) {

  # Circumvent warnings in build
  color <- NULL

  x <- df
  if (is.data.frame(df)) {
    x <- df[[field]]
  }

  tab <- rev(sort(table(x)))
  dfs <- data.frame(list(names = names(tab), count = tab))
  dfs <- dfs[dfs$count > 0,]

  # Show all cases if ntop not specified
  if (is.null(ntop)) {
    ntop <- nrow(dfs)
  }
  
  ntop <- min(ntop, nrow(dfs))
  dfs <- dfs[1:ntop,] # Pick top-n items
  dfs$names <- droplevels(factor(dfs$names, levels = rev(dfs$names)))

  dfs$color <- rep("black", nrow(dfs))
  if (!is.null(highlight)) {
    dfs$color <- rep("darkgray", nrow(dfs))
    dfs$color[dfs$names %in% highlight] <- "red"
  }

  theme_set(theme_bw(15))
  p <- ggplot(dfs, aes(x = names, y = count, fill = color))
  p <- p + geom_bar(stat = "identity")
  p <- p + coord_flip()
  p <- p + ylab(field) + xlab("")
  p

} 


