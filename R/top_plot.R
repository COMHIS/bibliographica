#' @title Plot Top Entries
#' @description Visualizes the top entries for a given field in a data frame.
#'   Count and percentage statistics is also shown as needed.
#' @param x Data frame, vector or factor
#' @param field Field to show
#' @param ntop Number of top entries to show
#' @param highlight Entries from the 'field' to be highlighted
#' @param max.char Max number of characters in strings. Longer strings will be cut and only max.char first characters are shown. No cutting by default
#' @param show.rest Show the count of leave-out samples (not in top-N) as an additional bar.
#' @param show.percentage Show the proportion of each category with respect to the total sample count.
#' @param log10 Show the counts on log10 scale (default FALSE)
#' @return ggplot object
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{p <- top_plot(x, field, 50)}
#' @keywords utilities
top_plot <- function (x, field = NULL, ntop = NULL, highlight = NULL, max.char = Inf, show.rest = FALSE, show.percentage = FALSE, log10 = FALSE) {

  # Circumvent warnings in build
  color <- percentage <- NULL

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
  topp <- sum(dfs$count)/sum(tab)

  if (show.rest & ntop < length(tab)) {
    dfs2 <- data.frame(list(names = "Other", count = sum(tab) - sum(dfs$count)))
    dfs <- bind_rows(dfs, dfs2)
  }

  # Limit length of names in the printout
  if (is.infinite(max.char)) {
    max.char <- max(nchar(as.character(dfs$names)))
  }

  levels1 <- length(unique(dfs$names))
  dfs$names <- substr(as.character(dfs$names), 1, max.char)
  levels2 <- length(unique(dfs$names))
  if (!levels1 == levels2) {
    warning("Truncating the names is mixing up some of the variable names.")
  }

  # Arrange levels; leave the leaveout category as the last one
  levs <- rev(unique(dfs$names))
  if ("Other" %in% levs) {
    levs <- c("Other", setdiff(levs, "Other"))
  }
  dfs$names <- droplevels(factor(dfs$names, levels = levs))
  dfs$percentage <- round(100 * dfs$count/sum(dfs$count), 1)

  dfs$color <- rep("black", nrow(dfs))
  if (!is.null(highlight)) {
    dfs$color <- rep("darkgray", nrow(dfs))
    dfs$color[dfs$names %in% highlight] <- "red"
    p <- ggplot(dfs, aes(x = names, y = count, fill = color))    
  } else {
    p <- ggplot(dfs, aes(x = names, y = count))
  }

  p <- p + geom_bar(stat = "identity", color = "black", fill = "white")
  p <- p + coord_flip()
  p <- p + xlab("") + ylab(paste(field, "(N)"))

  s <- paste("Total N=", sum(tab), " / Top-", ntop, ": ", round(100 * topp, 1), "%", sep = "")

  p <- p + labs(title = s)

  if (show.percentage) {
    p <- p + geom_text(aes(x = names, y = 30,
                           label = paste(percentage, "%", sep = "")))
  }


  if (log10) {
    p <- p + scale_y_log10() + labs(y = paste(field, "(N)"))
  }

  p

} 


