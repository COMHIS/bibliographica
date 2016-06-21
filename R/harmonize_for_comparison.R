#' @importFrom sorvi harmonize_names
harmonize_for_comparison <- function(x, language="english") {
  
  if (language=="swedish") {
    f <- system.file("extdata/sv_publisher_comparison.csv", package="bibliographica")
  } else if (language=="english") {
    
  } else if (language=="finnish") {
    f <- system.file("extdata/fi_publisher_comparison.csv", package="bibliographica")
  } else {
    
  }

  synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")
  
  x <- harmonize_names(x, synonyms, mode="recursive")
  x

}
