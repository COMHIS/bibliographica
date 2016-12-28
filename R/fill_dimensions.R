#' @title Fill Missing Dimensions
#' @description Estimate missing entries in dimension vector where possible.
#' @param x dimension string 
#' @param dimension.table Given mappings between document dimensions
#' @param sheet.dimension.table Given mappings for sheet dimensions
#' @return Augmented dimension vector
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @seealso augment_dimension_table
#' @examples \dontrun{
#'   dimension.table <- dimension_table(); 
#'   fill_dimensions(c(original = NA, gatherings = NA, width = 30, height = 48), dimension.table)
#' }
#' @keywords utilities
fill_dimensions <- function (x, dimension.table = NULL, sheet.dimension.table = NULL) {

    # Read dimension height/width/gatherings conversions
    if (is.null(dimension.table)) {
      dimension.table <- dimension_table()
    }
    if (is.null(sheet.dimension.table)) {
      sheet.dimension.table <- sheet_area(verbose = FALSE)
    }

    # Pick the available dimension information (some may be NAs)
    h <- as.numeric(as.character(x[["height"]]))
    w <- as.numeric(as.character(x[["width"]]))
    g <- as.character(x[["gatherings"]])
    o <- x[["original"]]

    if (!any("obl" == names(x))) {x[["obl"]] <- NA}
    obl <- x[["obl"]] 

    #if ((!any(g == colnames(dimension.table)) && (!is.na(g)))) {
    #  #warning(paste("gatherings ", g, " not available in dimension.table"))
    #}

    e <- estimate_document_dimensions(gatherings = g, height = h, width = w, obl = obl, dimension.table, sheet.dimension.table)

    w <- e$width
    h <- e$height
    g <- e$gatherings

    c(original = o, gatherings = g, width = w, height = h, obl = obl)

}

