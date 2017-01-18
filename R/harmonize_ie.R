#' @title Harmonize ie
#' @description Harmonize ie statements.
#' @param x A vector
#' @param separator The separator string (i.e by default)
#' @return A vector polished
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- harmonize_ie("i.e.")}
#' @keywords utilities
harmonize_ie <- function (x, separator = "i.e") {

  # Harmonized form
  h <- " i.e "

  x <- tolower(as.character(x))
  x <- condense_spaces(x)
  x <- gsub("-+", "-", x)

  x <- gsub("\\[oik[\\.]*", "[i.e ", x)
  x <- gsub("p\\.o\\.", " i.e ", x) # Finnish: pitaisi olla
  x <- gsub("p\\.o ", " i.e ", x) # Finnish: pitaisi olla  
  x <- gsub("\\[po[\\.]*", " i.e ", x) # Finnish: pitaisi olla  
  
  x <- gsub(" ie ", " i.e ", x)  
  x <- gsub("\\[ie ", "[i.e ", x)
  x <- gsub("\\[i\\. *e\\.* ", "[i.e ", x)
  x <- gsub("\\[i *e\\.* ", "[i.e ", x)
  x <- gsub("\\[i\\.e\\.* ", "[i.e ", x)      
  x <- gsub("^ie ", "i.e ", x)
  x <- gsub("i\\.e\\.*, ", "i.e ", x)      

  x <- gsub("\\,* +i\\.* *e+ *[\\.|\\,]*", h, x)

  x <- gsub("\\[* +i\\.* *e+ *[\\.|\\,]*", h, x)

  x <- gsub("^\\,* *i\\.* *e+ *[\\.|\\,]*", h, x)

  x <- gsub(" +i\\.* *e+ *[\\.|\\,]*", h, x)

  x <- gsub("p\\. i\\.* *e+ *[\\.|\\,]*", h, x) 
  x <- gsub("^p\\. i\\.* *e+ *[\\.|\\,]*", h, x)
  x <- gsub("\\[ *", "\\[", x)
  x <- gsub("^\\. *", "", x)

  x <- condense_spaces(x)

  x

}



