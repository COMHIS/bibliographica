#' @title Augment Dimension Table
#' @description Estimate missing entries in dimension table where possible.
#' @param dim.tab Dimension table.
#' @param dim.info Mapping between document dimensions.
#' @param sheet.dim.tab Sheet dimension table.
#' @param verbose verbose
#' @return Augmented dimension table.
#' @seealso polish_dimensions
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples # augment_dimension_table(dim.tab)
#' @keywords utilities
augment_dimension_table <- function (dim.tab, dim.info = NULL, sheet.dim.tab = NULL, verbose = FALSE) {

  width <- height <- gatherings <- NULL

  if (is.null(dim.info)) {
    if (verbose) {
      message("dim.info dimension mapping table not provided, using the default table dimension_table()")
    }
    dim.info <- dimension_table()
  }
  if (is.null(sheet.dim.tab)) {
    sheet.dim.tab <- sheet_area(verbose = verbose)
  }

  # Entry IDs
  if (verbose) {message("Unique dimension IDs.")}  
  id.orig <- apply(dim.tab, 1, function (x) {paste(as.character(x), collapse = "")})
  dim.tab.uniq <- unique(dim.tab)
  id.uniq <- apply(dim.tab.uniq, 1, function (x) {paste(as.character(x), collapse = "")})
  match.inds <- match(id.orig, id.uniq)
  rm(id.orig)

  if (verbose) {message(paste("Polishing dimension info:", nrow(dim.tab.uniq), "unique entries."))}

  # Only consider unique entries to speed up
  tmp <- NULL
  for (i in 1:nrow(dim.tab.uniq)) {
    if (verbose) {message(paste(i, "/", nrow(dim.tab.uniq)))}  
    tmp <- rbind(tmp, fill_dimensions(dim.tab.uniq[i, ], dim.info, sheet.dim.tab))
  }

  if (verbose) { message("Dimensions polished. Collecting the results.") }
  dim.tab.uniq <- as.data.frame(tmp)
  rm(tmp)
  rownames(dim.tab.uniq) <- NULL
  dim.tab.uniq$width <- as.numeric(as.character(dim.tab.uniq$width))
  dim.tab.uniq$height <- as.numeric(as.character(dim.tab.uniq$height))
  dim.tab.uniq$gatherings <- order_gatherings(dim.tab.uniq$gatherings)
  dim.tab.uniq$obl <- as.numeric(as.logical(dim.tab.uniq$obl))

  # print("Add area (width x height)")
  dim.tab.uniq <- mutate(dim.tab.uniq, area = width * height)

  # Map back to the original domain
  dim.tab.uniq[match.inds,]

}
