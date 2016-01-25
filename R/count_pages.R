count_pages <- function (z) {

  pp <- 0
  if (length(z) > 0) {
    if (seqtype(z) == "increasing.series") {
      pp <- intervalrule(z)
    } else {
      pp <- maxrule(z)
    }
  }
  pp

}
