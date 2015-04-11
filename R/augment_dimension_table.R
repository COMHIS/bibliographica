#' @title augment_dimension_table
#' @description Estimate missing entries in dimension table where possible
#'
#' @param dimension.table dimension.table
#' @return Augmented dimension.table
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' 
#' @examples # augment_dimension_table(dimension.table)
#' @keywords utilities
augment_dimension_table <- function (dimension.table) {

  dimtab.orig <- dimension.table			

  width <- height <- gatherings <- NULL			
  dimtab <- dimension_table()

  dimension.table <- as.data.frame(t(apply(dimension.table, 1, function (x) {fill_dimensions(x, dimtab)})))

  dimension.table$original <- as.character(dimension.table$original)
  dimension.table$width <- as.numeric(as.character(dimension.table$width))
  dimension.table$height <- as.numeric(as.character(dimension.table$height))

  # Order the levels
  glevels <- c("1to", "2small", "2to", "2long", "4small", "4to", "4long", "8to", "12to", "12long", "16to", "18to", "24to", "24long", "32to", "48to", "64to", NA)

  if (!all(unique(dimension.table$gatherings) %in% glevels)) { stop(paste("Add", setdiff(as.character(unique(dimension.table$gatherings)), glevels), "in gatherings levels in augment_dimension_table function")) }
  dimension.table$gatherings <- factor(dimension.table$gatherings, levels = glevels)

  # print("Add area (width x height)")
  dimension.table <- mutate(dimension.table, area = width * height)

  dimension.table

}
