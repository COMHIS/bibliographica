count_pages <- function (z) {

  pp <- 0
  if (length(z) > 0) {
    stype <- seqtype(z)

    if (stype == "increasing.series") {
      pp <- intervalrule(z)
    } else {
      pp <- maxrule(z)
    }
  }
  pp

}
