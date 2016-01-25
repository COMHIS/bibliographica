remove.squarebrackets <- function (x) {

  rm.sqb <- function (x) {		      
    str_trim(gsub("\\[", "", gsub("\\]", "", x)))
  }

  x <- sapply(x, function (x) {rm.sqb(x)})

  x
}

