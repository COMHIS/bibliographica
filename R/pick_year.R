pick_year <- function (x) {

  # 1002[1000] -> 1000
  strsplit(gsub("\\]", "", x), "\\[")[[1]][[2]]

}