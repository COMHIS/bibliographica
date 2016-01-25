#' @title augment_dimension_table
#' @description Estimate missing entries in dimension table where possible
#' @param dimension.table dimension.table
#' @param dimtab Mapping between document dimensions.
#' @param verbose verbose
#' @return Augmented dimension.table
#' @seealso polish_dimensions
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @importFrom dplyr mutate
#' @examples # augment_dimension_table(dimension.table)
#' @keywords utilities
augment_dimension_table <- function (dimension.table, dimtab, verbose = FALSE, sheet.dimension.table) {

  dimtab.orig <- dimension.table
  width <- height <- gatherings <- NULL

  tmp <- NULL
  for (i in 1:nrow(dimension.table)) {
    x <- dimension.table[i, ]

    tmp2 <- fill_dimensions(x, dimtab, sheet.dimension.table)

    tmp <- rbind(tmp, tmp2)
  }

  dimension.table <- as.data.frame(tmp)
  dimension.table$original <- as.character(dimension.table$original)
  dimension.table$width <- as.numeric(as.character(dimension.table$width))
  dimension.table$height <- as.numeric(as.character(dimension.table$height))
  dimension.table$gatherings <- order_gatherings(dimension.table$gatherings)
  dimension.table$obl <- as.numeric(dimension.table$obl)

  # print("Add area (width x height)")
  dimension.table <- mutate(dimension.table, area = width * height)

  dimension.table

}
