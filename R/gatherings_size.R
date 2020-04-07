#' @title Gatherings Size
#' @description Return size of a given document format (gatherings) in cm.
#' @param x Gatherings name 
#' @param sheet.dimensions Mapping between document dimensions.
#' @return Vector (single input) or matrix (multiple inputs)
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples d <- gatherings_size("Octavo")
#' @keywords utilities
gatherings_size <- function (x, sheet.dimensions = NULL) {

  width <- height <- gatherings <- NULL
  if (is.null(sheet.dimensions)) {
    sheet.dimensions <- sheet_area()
  }

  # Identify whether we have names or codes
  name <- mean(x %in% sheet.dimensions$format)
  code <- mean(x %in% sheet.dimensions$gatherings)  
  if (name > code) {
    field <- "format"
  } else {
    field <- "gatherings"
  }
  sheet.dimensions$field <- sheet.dimensions[[field]]

  x <- tolower(x)

  xorig <- x
  xuniq <- unique(x)
  match.inds <- match(xorig, xuniq)
  d <- sheet.dimensions[match(xuniq, sheet.dimensions$field), ]
  d <- d[match.inds,]
  rownames(d) <- NULL
  d

}
