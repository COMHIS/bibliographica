is.decreasing <- function (x) {

  # Remove dashes
  x <- na.omit(suppressWarnings(as.numeric(unlist(strsplit(x, "-"), use.names = FALSE))))

  # Test if the numeric series is decreasing
  decr <- FALSE
  if (!all(is.na(x))) {
    decr <- all(diff(x) <= 0)
  }

  decr
}
