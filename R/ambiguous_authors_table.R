#' @title Ambiguous authors table
#' @description Read table of ambiguous author name synonymes
#' @param file Input file
#' @return Author synonyme data frame with fields 'name' and 'synonyme' 
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples aa <- ambiguous_authors_table()
#' @keywords utilities
ambiguous_authors_table <- function (file = NULL) {

  # Read author synonymes for ambiguous authors
  if (is.null(file)) {
    file <- system.file("extdata/ambiguous-authors.csv", package = "bibliographica")
  }
  aa <- readLines(file)[-1]
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

