
sheets2pages <- function (x) {

  sheets2pages.single <- function (x) {
    if (x == "sheet") {x <- "1 sheet"}
    if (x == "sheets") {x <- "2 sheets"}
    str_trim(unlist(strsplit(x, "sheet"), use.names = FALSE)[[1]]) 
  }

  inds <- grep("sheet", x)
  if (length(inds) > 0) {
    # 1 sheet = 2 pages
    pages <- sapply(x[inds], function (x) {sheets2pages.single(x)}, USE.NAMES = FALSE)
    pages <- suppressWarnings(as.numeric(as.roman(pages)))
    x[inds] <- 2*pages
  }

  as.numeric(x)

}
