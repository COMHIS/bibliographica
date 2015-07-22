#' Harmonize print statements
#'
#' @param x a vector
#' @return Harmonized vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples # x2 <- harmonize_print_statements("prentyd in London")$name
#' @keywords utilities
harmonize_print_statements <- function (x) {

  x <- as.character(x)			   
  xorig <- x

  ### Get printing terms from tables in various languages

  for (lang in c("finnish", "english", "french", "german", "swedish")) {

    f <- system.file(paste0("extdata/printterms_", lang, ".csv"), package = "bibliographica")
    
    terms <- read.csv(f, sep = "|")

    # Add capitalized versions
    terms2 <- terms
    terms2$synonyme <- capitalize(terms$synonyme)
    terms <- rbind(terms, terms2)

    # Harmonize the terms
    x <- as.character(harmonize_names(x, terms, mode = "recursive")$name)
  }
  
  x <- condense_spaces(x)

  list(name = x, original = xorig)

}
