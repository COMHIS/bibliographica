#' @title Polish publisher 
#' @description Polish publisher field separating for/by
#'
#' @param x Publisher vector
#' @return Polished vector
#'
#' @export
#' @importFrom sorvi condense_spaces
#' @importFrom sorvi harmonize_names
#' @details Polish publisher field. 
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples # polish_publisher("printed and sold by R. Marchbank")
#' @keywords utilities
polish_publisher_forby <- function (x) {

  x <- as.character(x)
  xorig <- x
  x <- gsub("\\[", "", x)
  x <- gsub("\\]", "", x)
  x <- gsub("\\.$", "", x)
  x <- gsub("\\,$", "", x)
  x <- gsub("^s\\.n$", "s\\.n\\.", x)

  # Harmonize print statements
  xh <- harmonize_print_statements(x)

  # Pick fields by taking the occurrence between
  # printed for and the first comma (printed for N.N, -> N.N)
  # This is a drastic simplification but speeds up preprocessing
  # by orders of magnitude
  x <- xh$name
  xfor <- pick_print_fields(x, " printed for")
  xby <- pick_print_fields(x, " printed by")

  # List the remaining polished entries where "print for" or "print by" 
  # statements were not found 
  x <- condense_spaces(x)
  x[x == ""] <- NA
  xrest <- x
  xrest[which(!is.na(xfor) | !is.na(xby))] <- NA

  res <- list(original = xorig, printedfor = xfor, printedby = xby, rest = xrest)

  res
 
}


#' @title pick_print_fields
#' @description Pick print fields
#'
#' @param x A vector
#' @param field field names
#' @return fields
#'
#' @export
#' @importFrom bibliographica condense_spaces
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' 
#' @examples \dontrun{x2 <- pick_print_fields(x, field)}
#' @keywords utilities
pick_print_fields <- function (x, field) {

  # Init		  
  fields <- rep(NA, length(x))		  

  # Pick entries with this field
  inds <- grep(field, x) 
  
  if (length(inds) > 0) {

    # Split per field and comma/semicolon; take the occurrence in between
    txt <- sapply(strsplit(x[inds], field), function (x) {x[[2]]})
    txt <- sapply(strsplit(txt, ","), function (x) {x[[1]]})
    # Avoid splitting problems
    txt[txt == ""] <- " "
    txt <- sapply(strsplit(txt, ";"), function (x) {x[[1]]})

    # Remove extra points
    txt <- gsub("\\.", " ", txt)

    # Remove extra statements
    txt <- gsub("and sold by", "", txt)

    # Remove place statements
    for (statement in c(" in", " at", " and sold by", " near")) {
      txt <- sapply(strsplit(txt, statement), function (x) {x[[1]]})
      txt[txt == ""] <- " "
    }

    txt <- condense_spaces(txt)

    fields[inds] <- txt

  }

  fields
}


