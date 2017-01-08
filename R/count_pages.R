count_pages <- function (z) {

  pp <- 0
  if (length(z) > 0) {
    if (seqtype(z) == "increasing.series") {
      pp <- intervalrule(z)
    } else if (seqtype(z) == "decreasing.series") {
      pp <- intervalrule(z, revert = TRUE)
    } else {
      pp <- maxrule(z)
    }
  }
  pp

}
