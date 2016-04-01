remove.squarebrackets <- function (x, mc.cores = 1) {

  rm.sqb <- function (x) {		      
    str_trim(gsub("\\[", "", gsub("\\]", "", x)))
  }

  #x <- sapply(x, function (x) {rm.sqb(x)})
  x <- unlist(mclapply(x, function (x) {rm.sqb(x)}, mc.cores = mc.cores))

  x
}

