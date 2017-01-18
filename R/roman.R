is.roman <- function (x) {

  x <- gsub("\\.", NA, x)

  check.roman <- function (x) {

    if (x == "" || is.na(x)) {return(FALSE)}

    xs <- unlist(strsplit(x, "-"), use.names = FALSE)
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

  sapply(x, check.roman, USE.NAMES = FALSE)

}




roman2arabic <- function (x) {

  if (length(grep("vj", x)) == 1) {
    x <- gsub("j", "i", x)
  }

  helpf <- function(xi) {

    # Return numeric hits immediately (such as "4387")
    if (grepl("^[0-9]+$", xi)) {
      xr <- xi
    } else if (length(grep("-", xi)) > 0) {
      x2 <- str_trim(unlist(strsplit(xi, "-"), use.names = FALSE))
      n <- suppressWarnings(as.numeric(as.roman(x2)))
      n[is.na(n)] <- x2[is.na(n)] # vii-160
      xr <- paste(n, collapse = "-")
    } else {
      xr <- suppressWarnings(as.numeric(as.roman(xi)))
    }
    xr
  }

  sapply(x, function (xi) {helpf(xi)}, USE.NAMES = FALSE)

}


