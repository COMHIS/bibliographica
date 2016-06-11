
double_pages <- function (x) {

  if (length(x) == 0) {return (0)}	     

  # x is a vector of page number strings
  # If some pages are separated by "-" then just pick the maximum page number
  x <- as.numeric(unlist(strsplit(x, "-"), use.names = FALSE))
  x <- str_trim(x)
  x <- as.numeric(as.vector(na.omit(x)))

  max(x)

}

