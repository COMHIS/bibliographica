harmonize_page_info <- function (s) {

  f <- system.file("extdata/harmonize_page_info.csv", package = "bibliographica")
  harm <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))  

  # Harmonize
  for (i in 1:nrow(harm)) {
    s <- gsub(harm$synonyme[[i]], harm$name[[i]], s)
  }  


  s

}
