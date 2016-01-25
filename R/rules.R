
maxrule <- function (x) {
  max(na.omit(suppressWarnings(as.numeric(unlist(strsplit(x, "-"), use.names = FALSE)))))
}

intervalrule <- function (x) {
  xx <- na.omit(suppressWarnings(as.numeric(unlist(strsplit(x, "-"), use.names = FALSE))))
  max(xx) - min(xx) + 1
}
