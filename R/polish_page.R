#' @title Polish Page
#' @description Clean up page numbers for a single document.
#' @param x Page number field. Vector or factor of strings.
#' @return Cleaned up version of page number field.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @importFrom stringr str_trim
#' @examples \dontrun{polish_page("4p.")}
#' @keywords internal
polish_page <- function (x) {

  # "5v. 3-20, [5]" -> "3-20, [5]"
  s <- suppressWarnings(remove_volume_info(s))

  # No infinite page counts
  pages[is.infinite(pages)] <- NA
      
  # TODO raw not needed in output
  #list(raw = s, pages = pages)
  unname(pages)

}

