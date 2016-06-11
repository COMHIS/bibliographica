position_romans <- function (x) {
  sapply(gsub("^\\[", "", gsub("\\]$", "", x)), function (xi) {any(is.roman(unlist(strsplit(xi, "-"), use.names = FALSE)))})
}
