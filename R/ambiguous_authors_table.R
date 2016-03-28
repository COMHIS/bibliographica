#' @title Ambiguous authors table
#' @description Read table of ambiguous author name synonymes
#' @param ... Arguments to be pased
#' @return Author synonyme data frame with fields 'name' and 'synonyme' 
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' @examples aa <- ambiguous_authors_table()
#' @keywords utilities
ambiguous_authors_table <- function (...) {

  # Read author synonymes for ambiguous authors
  f <- system.file("extdata/ambiguous-authors.csv", package = "estc")
  aa <- readLines(f)[-1]
  aa <- lapply(aa, function (x) {unlist(strsplit(x, ";"))})
  names(aa) <- sapply(aa, function (x) {rev(x)[[1]]})
  map <- NULL
  for (nam in names(aa)) {
    map <- rbind(map, cbind(rep(nam, length(aa[[nam]])), aa[[nam]]))
  }
  aa <- as.data.frame(map)
  names(aa) <- c("name", "synonyme")

  aa 

}

