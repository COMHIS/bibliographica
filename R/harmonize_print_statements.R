#' @title harmonize_print_statements
#' @description Harmonize print statements
#' @param x a vector
#' @return Harmonized vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # x2 <- harmonize_print_statements("prentyd in London")$name
#' @keywords utilities
harmonize_print_statements <- function (x) {

  x <- as.character(x)			   
  xorig <- x

  ### Get printing terms from tables in various languages

  for (lang in c("finnish", "french", "german", "swedish", "english")) {
print(lang)
    f <- system.file(paste0("extdata/printterms_", lang, ".csv"), package = "bibliographica")
    terms <- read.csv(f, sep = "|")
    x <- tolower(x)

    # Harmonize the terms
    x <- as.character(harmonize_names(x, terms, mode = "recursive", check.synonymes = FALSE)$name)
    x <- condense_spaces(x)    

  }

  list(name = x, original = xorig)

}
