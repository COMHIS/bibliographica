get_pseudonymes <- function (...) {
  pseudo1 <- as.character(read.csv(system.file("extdata/stopwords_pseudonymes.csv", package = "bibliographica"), sep = "\t")[,1])
  pseudo2 <- as.character(read.csv(system.file("extdata/names/pseudonymes/first.csv", package = "bibliographica"), sep = "\t")[,1])
  pseudo3 <- as.character(read.csv(system.file("extdata/names/pseudonymes/last.csv", package = "bibliographica"), sep = "\t")[,1])
  pseudo <- sort(unique(c(pseudo1, pseudo2, pseudo3)))

  pseudo

}
