#' @title Plot Top Entries
#' @description Plot the top entries for a given field in a data frame.
#' @param x Data frame, vector or factor
#' @param field Field to show
#' @param ntop Number of top entries to show
#' @param highlight Entries from the 'field' to be highlighted
#' @param max.char Max number of characters in strings. Longer strings will be cut and only max.char first characters are shown. No cutting by default
#' @return ggplot object
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{p <- top_plot(x, field, 50)}
#' @keywords utilities
top_plot <- function (x, field = NULL, ntop = NULL, highlight = NULL, max.char = Inf) {

  # Circumvent warnings in build
  color <- NULL

  if (is.data.frame(x)) {
    x <- x[[field]]
  }

  if (is.factor(x) || is.character(x) || is.numeric(x)) {
    x <- droplevels(as.factor(x))
  }

  if (length(x) == 0) {
    return(ggplot())
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

  # Limit length of names in the printout
  if (is.infinite(max.char)) {
    max.char <- max(nchar(as.character(dfs$names)))
  }

  levels1 <- length(unique(dfs$names))
  dfs$names <- substr(as.character(dfs$names), 1, max.char)
  levels2 <- length(unique(dfs$names))
  if (!levels1 == levels2) {warning("Truncating the names is mixing up some of the variable names.")}

  dfs$names <- droplevels(factor(dfs$names, levels = rev(unique(dfs$names))))

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


