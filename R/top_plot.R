#' @title Plot Top Entries
#' @description Plot the top entries for a given field in a data frame.
#' @param x Data frame, vector or factor
#' @param field Field to show
#' @param ntop Number of top entries to show
#' @param highlight Entries from the 'field' to be highlighted
#' @return ggplot object
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{p <- top_plot(x, field, 50)}
#' @keywords utilities
top_plot <- function (x, field = NULL, ntop = NULL, highlight = NULL) {

  if (nrow(x) == 0 | length(x) == 0) {
    return(ggplot())
  }

  # Circumvent warnings in build
  color <- NULL

  if (is.data.frame(x)) {
    x <- x[[field]]
  }

  if (is.factor(x)) {
    x <- droplevels(x)
  }

  tab <- rev(sort(table(x)))
  tab <- tab[tab > 0]
  
  dfs <- data.frame(names = names(tab), count = as.numeric(tab))

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
    p <- ggplot(dfs, aes(x = names, y = count, fill = color))    
  } else {
    p <- ggplot(dfs, aes(x = names, y = count))
  }

  theme_set(theme_bw(15))
  p <- p + geom_bar(stat = "identity")
  p <- p + coord_flip()
  p <- p + ylab(field) + xlab("")
  p

} 


