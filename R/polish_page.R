#' @title polish_page
#' @description Clean up page numbers for a single document.
#'
#' @param x Page number field. Vector or factor of strings.
#' @return Cleaned up version of page number field.
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @importFrom stringr str_trim
#' 
#' @examples \dontrun{polish_page("4p.")}
#' @keywords internal
polish_page <- function (x) {

  # Convert to string 	    	    
  s <- as.character(x)

  # Remove volume info
  # "5v. 3-20, [5]" -> "3-20, [5]"
  s <- suppressWarnings(remove_volume_info(s))

  # Volumes are separated by semicolons
  # Split by semicolon to list each volume separately
  spl <- str_trim(unlist(strsplit(s, ";")))

  if (length(spl) > 0) {
    # Assess pages per volume
    pages <- sapply(spl, function (x) { estimate_pages(x) })
  } else {
    pages <- NA
  }

  list(raw = spl, pages = pages)
 
}

