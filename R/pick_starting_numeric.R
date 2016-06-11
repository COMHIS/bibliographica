
pick_starting_numeric <- function (x) {

  num <- TRUE	
  n <- 0
  i <- 0

  while (!is.na(n) && i < nchar(x)) {
    i <- i+1
    ss <- substr(x, 1, i)
    n <- suppressWarnings(as.numeric(ss))
    if (!is.na(n)) {
      num <-n 
    }
  }

  num

}



