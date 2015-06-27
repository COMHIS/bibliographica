
maxrule <- function (x) {
  x <- unlist(strsplit(x, "-"))
  max(na.omit(suppressWarnings(as.numeric(x))))
}

intervalrule <- function (x) {
  x <- unlist(strsplit(x, "-"))
  xx <- na.omit(suppressWarnings(as.numeric(x)))
  max(xx) - min(xx) + 1
}
