#' @title fill_dimensions
#' @description Estimate missing entries in dimension vector where possible
#'
#' @param x dimension string 
#' @param dimension.table Given mappings between document dimensions
#' @return Augmented dimension vector
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' 
#' @seealso augment_dimension_table
#' @examples # dimension.table <- dimension_table(); 
#'           #  fill_dimensions(c(original = NA, gatherings = NA, 
#'	     #			  width = 30, height = 48), 
#'	     #			  dimension.table)
#' @keywords utilities
fill_dimensions <- function (x, dimension.table) {

    # Pick the available dimension information (some may be NAs)
    h <- as.numeric(as.character(x[["height"]]))
    w <- as.numeric(as.character(x[["width"]]))
    g <- as.character(x[["gatherings"]])
    o <- x[["original"]]

    if (!g %in% colnames(dimension.table) && !is.na(g)) {
      warning(paste("gatherings ", g, " not available in dimension.table"))
    }
    
    # Estimate document widths and heights from conversion table when not available
    e <- estimate_document_dimensions(gatherings = g, height = h, width = w, dimension.table)
    w <- e$width
    h <- e$height
    g <- e$gatherings

    c(original = o, gatherings = g, width = w, height = h)

}
