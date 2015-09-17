#' @title polish_publisher
#' @description Polish publishing house names
#'
#' @param x publisher field (a vector)
#' @return polished publisher field (a vector)
#'
#' @export
#' 
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{v <- polish_publisher(c("Oxford University Press","tryckt hos Cambridge University Press"))}
#' @keywords utilities
polish_publisher <- function(x) {
  x <- gsub("typis ","",x)
  x <- gsub("impressit ","",x)
  x <- gsub("impress. ","",x)
  x <- gsub("viduam ","",x)
  x <- gsub("viduae ","",x)
  x <- gsub("impr.","",x)
  x <- gsub("excud. ","",x)
  x <- gsub("exc.","",x)
  x <- gsub("tryckt af ","",x)
  x <- gsub("tryckt hos ","",x)
  x <- gsub("direct. ","",x)
  x <- gsub("mebat ","",x)
  x <- gsub("prändätty ","",x)
  x <- gsub("tryckt i ","",x)
  x <- gsub("^s.n$",NA,x)
  x <- gsub("^s. n$",NA,x)

  x
}
