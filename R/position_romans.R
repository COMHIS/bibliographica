position_romans <- function (x) {

  positions <- rep(FALSE, length(x))
  for (i in 1:length(x)) {
    spl <- gsub("^\\[", "", gsub("\\]$", "", unlist(strsplit(x[[i]], "-"))))
    if (any(sapply(spl, is.roman))) {
      positions[[i]] <- TRUE
    }
  }

  list(positions = positions)

}
