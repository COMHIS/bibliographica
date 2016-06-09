decapitate_keywords <- function(x, full_name, languages=c("english")) {
  
  # TODO: make a table for languages & related files & purpose
  f <- vector(length=length(languages))
  i <- 1
  if ("finnish" %in% languages) {
    f[i] <- "inst/extdata/fi_lowercase_keywords.csv"
    i <- i + 1
  }
  if ("swedish" %in% languages) {
    f[i] <- "inst/extdata/sv_lowercase_keywords.csv"
    i <- i + 1
  }
  if ("english" %in% languages) {
    f[i] <- "inst/extdata/en_lowercase_keywords.csv"
    i <- i + 1
  }
  if ("latin" %in% languages) {
    f[i] <- "inst/extdata/la_lowercase_keywords.csv"
    i <- i + 1
  }
  
  for (fil in f) {
    synonyms <- read.csv(file=fil, sep="\t", fileEncoding="UTF-8")
    for (i in 1:nrow(synonyms)) {
      x <- str_replace(x, as.character(synonyms$synonyme[i]), as.character(synonyms$name[i]))
    }
  }
  x
  
}