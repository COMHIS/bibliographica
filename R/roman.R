is.roman <- function (x) {

  x <- gsub("\\.", NA, x)

  check.roman <- function (x) {

    if (x == "" || is.na(x)) {return(FALSE)}

    xs <- unlist(strsplit(x, "-"))
    isr <- c()

    for (i in 1:length(xs)) {  
      x <- xs[[i]]
      tmp <- suppressWarnings(as.numeric(x))
      tmp2 <- suppressWarnings(as.numeric(as.roman(x)))
      not.numeric <- length(na.omit(tmp)) > 0
      roman.numeric <- is.numeric(tmp2)

      isr[[i]] <- !(not.numeric && roman.numeric) && !is.na(tmp2) 
    }
    # iii-7 TRUE; iii-iv TRUE; 4-7 FALSE
    any(isr)
  }

  sapply(x, check.roman)

}



roman2arabic <- function (x) {

  for (i in 1:length(x)) {

    xi <- x[[i]]

    if (length(grep("-", xi)) > 0) {
      x2 <- str_trim(unlist(strsplit(xi, "-")))
      n <- suppressWarnings(as.numeric(as.roman(x2)))
      n[is.na(n)] <- x2[is.na(n)] # vii-160
      xr <- paste(n, collapse = "-")
    } else {
      xr <- suppressWarnings(as.numeric(as.roman(xi)))
    }

    x[[i]] <- xr

  }

  x 

}


