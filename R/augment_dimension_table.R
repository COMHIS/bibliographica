#' @title Augment dimension table
#' @description Estimate missing entries in dimension table where possible
#' @param dimension.table dimension.table
#' @param dimtab Mapping between document dimensions.
#' @param verbose verbose
#' @param sheet.dimension.table sheet.dimension.table
#' @return Augmented dimension.table
#' @seealso polish_dimensions
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @importFrom dplyr mutate
#' @examples # augment_dimension_table(dimension.table)
#' @keywords utilities
augment_dimension_table <- function (dimension.table, dimtab = NULL, verbose = FALSE, sheet.dimension.table = NULL) {

  dimtab.orig <- dimension.table
  width <- height <- gatherings <- NULL

  if (is.null(dimtab)) {
    if (verbose) {
      message("dimtab dimension mapping table not provided, using the default table dimension_table()")
    }
    dimtab <- dimension_table()
  }
  if (is.null(sheet.dimension.table)) {
    sheet.dimension.table <- sheet_area(verbose = verbose)
  }

  tmp <- NULL
  for (i in 1:nrow(dimension.table)) {
    x <- dimension.table[i, ]

    tmp2 <- fill_dimensions(x, dimtab, sheet.dimension.table)

    tmp <- rbind(tmp, tmp2)
  }

  dimension.table$original <- as.character(dimension.table$original)
  dimension.table$width <- as.numeric(as.character(dimension.table$width))
  dimension.table$height <- as.numeric(as.character(dimension.table$height))
  dimension.table$gatherings <- order_gatherings(dimension.table$gatherings)
  dimension.table$obl <- as.numeric(as.logical(dimension.table$obl))

  # print("Add area (width x height)")
  dimension.table <- mutate(dimension.table, area = width * height)

  dimension.table

}
