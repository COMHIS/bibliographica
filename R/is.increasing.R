is.increasing <- function (x) {

  # Ignore square brackets when determining increasing sequence
  #x <- x[!pagecount.attributes["squarebracket",]]

  # Ignore starting romans when determining increasing sequence
  #x <- x[!pagecount.attributes["roman.start",]]

  # Remove dashes
  x <- na.omit(suppressWarnings(as.numeric(unlist(strsplit(x, "-"), use.names = FALSE))))

  # Test if the numeric series is increasing
  incr <- FALSE
  if (!all(is.na(x))) {
    incr <- all(diff(x) >= 0)
  }

  incr
}
