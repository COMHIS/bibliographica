get_pseudonymes <- function (...) {
  pseudo1 <- as.character(read.csv(system.file("extdata/pseudonymes.csv", package = "bibliographica"), sep = "\t")[,1])
  pseudo2 <- as.character(read.csv(system.file("extdata/names/pseudonymes/first.csv", package = "bibliographica"), sep = "\t")[,1])
  pseudo3 <- as.character(read.csv(system.file("extdata/names/pseudonymes/last.csv", package = "bibliographica"), sep = "\t")[,1])
  pseudo <- c(pseudo1, pseudo2, pseudo3)

  # Remove extra spaces
  pseudo <- condense_spaces(pseudo)
  # Also consider removing periods, commas, dashes etc and taking lowercase ?

  # Organize
  pseudo <- sort(unique(pseudo))

  pseudo

}
