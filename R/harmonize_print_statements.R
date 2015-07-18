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

  ### Get printing terms from tables in various languages

  # Finnish
  f <- system.file("extdata/printterms_finnish.csv", package = "bibliographica")
  terms.fi <- as.character(read.csv(f)[,1])
  x <- harmonize_names(x, terms.fi, mode = "recursive")$name
  
  # English
  f <- system.file("extdata/printterms_english.csv", package = "bibliographica")
  terms.en <- read.csv(f, sep = "|")
  x <- harmonize_names(x, terms.en, mode = "recursive")$name

  # French
  f <- system.file("extdata/printterms_french.csv", package = "bibliographica")
  terms.fr <- read.csv(f, sep = "|")
  x <- harmonize_names(x, terms.fr, mode = "recursive")$name

  # German
  f <- system.file("extdata/printterms_german.csv", package = "bibliographica")
  terms.ge <- read.csv(f, sep = "|")
  x <- harmonize_names(x, terms.ge, mode = "recursive")$name

  # Swedish
  f <- system.file("extdata/printterms_swedish.csv", package = "bibliographica")
  terms.se <- read.csv(f, sep = "|")
  x <- harmonize_names(x, terms.se, mode = "recursive")$name

  x

}
