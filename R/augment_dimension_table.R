#' @title augment_dimension_table
#' @description Estimate missing entries in dimension table where possible
#'
#' @param dimension.table dimension.table
#' @param dimtab Mapping between document dimensions.
#' @param verbose verbose
#' @return Augmented dimension.table
#'
#' @seealso polish_dimensions
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @importFrom dplyr mutate
#' @examples # augment_dimension_table(dimension.table)
#' @keywords utilities
augment_dimension_table <- function (dimension.table, dimtab = NULL, verbose = FALSE) {

  dimtab.orig <- dimension.table
  width <- height <- gatherings <- NULL

  if (is.null(dimtab)) {
    if (verbose) {
      message("dimtab dimension mapping table not provided, using the default table dimension_table()")
    }
    dimtab <- dimension_table()
  }

  # Return to this after the debug
  tmp <- apply(dimension.table, 1, function (x) {fill_dimensions(x, dimtab)})
  #tmp <- NULL
  #for (i in 1:nrow(dimension.table)) {
  #  x <- dimension.table[i,]
  #  print(x)
  #  tmp <- rbind(tmp, fill_dimensions(x, dimtab))
  #}
  
  dimension.table <- as.data.frame(t(tmp))
  dimension.table$original <- as.character(dimension.table$original)
  dimension.table$width <- as.numeric(as.character(dimension.table$width))
  dimension.table$height <- as.numeric(as.character(dimension.table$height))
  dimension.table$gatherings <- order_gatherings(dimension.table$gatherings)

  # print("Add area (width x height)")
  dimension.table <- mutate(dimension.table, area = width * height)

  dimension.table

}
