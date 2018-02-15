#' @title Field availability
#' @description Estimate fraction of missing entries (NAs) and visualize data availability for the different fields.
#' @param df data.frame to explore
#' @return List with following elements
#' \itemize{
#'   \item{plot}{ggplot2 object}
#'   \item{table}{Table summarizing the field name (field_name), overall fraction of available (non-NA) and missing entries (NAs) as percentages [0, 100], number of non-NA entries (n), and the number of unique entries (unique_entries)}
#' }
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # field_availability(df)
#' @keywords utilities
field_availability <- function (df) {

  field <- available <- NULL

  ava <- data.frame(field_name = colnames(df))
  ava$n <- colSums(!is.na(df))  
  ava$available <- 100*ava$n/nrow(df)
  ava$missing <- 100 - ava$available
  ava$unique_entries <- apply(df, 2, function (x) {length(unique(x))})

  df2 <- ava[, c("available", "field_name")]
  df2$field <- factor(df2$field, levels = df2$field[order(df2$available)])

  theme_set(theme_bw(15))
  p <- ggplot(df2, aes(x = field, y = available))
  p <- p + geom_bar(stat = "identity")
  p <- p + xlab("")
  p <- p + ylab("Fraction of available entries (%)")
  p <- p + ggtitle("Data availability")
  p <- p + coord_flip()

  list(plot = p, table = ava)
}

