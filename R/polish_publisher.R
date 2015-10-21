#' @title polish_publisher
#' @description Polish publishing house names
#'
#' @param x publisher field (a vector)
#' @param synonyms Synonyme table
#' @return polished publisher field (a vector)
#'
#' @export
#' 
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{v <- polish_publisher(c("Oxford University Press","tryckt hos Cambridge University Press"))}
#' @keywords utilities
polish_publisher <- function(x, synonyms=NULL) {
  
  if (is.null(synonyms)) {
    f <- system.file("extdata/publisher.csv", package = "bibliographica")
    synonyms <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
  }
  
  x <- harmonize_names(x, synonyms)
  
  #x <- gsub("typis ","",x)
  #x <- gsub("impress. ","",x)
  #x <- gsub("viduam ","",x)
  #x <- gsub("viduae ","",x)
  #x <- gsub("impr.","",x)
  #x <- gsub("excud. ","",x)
  #x <- gsub("exc.","",x)
  #x <- gsub("tryckt af ","",x)
  #x <- gsub("tryckt hos ","",x)
  #x <- gsub("direct. ","",x)
  #x <- gsub("mebat ","",x)
  #x <- gsub("prändätty ","",x)
  #x <- gsub("tryckt i ","",x)
  #x <- gsub("^s.n$",NA,x)
  #x <- gsub("^s. n$",NA,x)

  x
}
