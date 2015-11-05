#' @importFrom sorvi harmonize_names
#' 

#f <- system.file("extdata/translation_sv_en_publisher.csv", package = "bibliographica")
harmonize_for_comparison <- function(x, language="english") {
  

  if (language=="swedish") {
    f <- "../inst/extdata/sv_publisher_comparison.csv"
  } else if (language=="english") {
    
  } else {
    
  }

  synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")
  synonyms$synonyme <- paste(synonyms$synonyme, "\\b",sep = "")

  q <- harmonize_names(x, synonyms, mode="recursive")$name
  q

}