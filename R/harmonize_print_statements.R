#' @title Harmonize Print Statements
#' @description Harmonize print statements.
#' @param x a vector
#' @param lowercase Set all in lowercase
#' @return Harmonized vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # x2 <- harmonize_print_statements("prentyd in London")
#' @keywords utilities
harmonize_print_statements <- function (x, lowercase = FALSE) {

  x <- as.character(x)

  xorig <- x

  if (lowercase) {x <- tolower(x)}

  ### Get printing terms from tables in various languages

  for (lang in c("finnish", "french", "german", "swedish", "english")) {
    f <- system.file(paste0("extdata/printterms_", lang, ".csv"), package = "bibliographica")
    terms <- read_mapping(f, sep = "|", mode = "table")
    x <- tolower(x)

    # Harmonize the terms
    x <- as.character(map(x, terms, mode = "recursive"))
    x <- condense_spaces(x)

  }

  list(name = x, original = xorig)

}
