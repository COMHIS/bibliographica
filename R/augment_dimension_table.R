#' @title augment_dimension_table
#' @description Estimate missing entries in dimension table where possible
#'
#' @param dimension.table dimension.table
#' @param dimtab Mapping between document dimensions.
#' @return Augmented dimension.table
#'
#' @seealso polish_dimensions
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples # augment_dimension_table(dimension.table)
#' @keywords utilities
augment_dimension_table <- function (dimension.table, dimtab = NULL) {

  dimtab.orig <- dimension.table			

  width <- height <- gatherings <- NULL			

  if (is.null(dimtab)) {
    message("dimtab dimension mapping table not provided, using the default table dimension_table()")
    dimtab <- dimension_table()
  }

  dimension.table <- as.data.frame(t(apply(dimension.table, 1, function (x) {fill_dimensions(x, dimtab)})))

  dimension.table$original <- as.character(dimension.table$original)
  dimension.table$width <- as.numeric(as.character(dimension.table$width))
  dimension.table$height <- as.numeric(as.character(dimension.table$height))

  dimension.table$gatherings <- order_gatherings(dimension.table$gatherings)

  # print("Add area (width x height)")
  dimension.table <- mutate(dimension.table, area = width * height)

  dimension.table

}
