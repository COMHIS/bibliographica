get_relation_keyword <- function(x, full_name, languages=c("english")) {
  
  # TODO: make a table for languages & related files & purpose
  f <- vector(length=length(languages))
  i <- 1
  if ("finnish" %in% languages) {
    f[i] <- system.file("extdata/fi_relation_keywords.csv", package="bibliographica")
    i <- i + 1
  }
  if ("swedish" %in% languages) {
    f[i] <- system.file("extdata/sv_relation_keywords.csv", package="bibliographica")
    i <- i + 1
  }
  if ("english" %in% languages) {
    f[i] <- system.file("extdata/en_relation_keywords.csv", package="bibliographica")
    i <- i + 1
  }
  if ("latin" %in% languages) {
    f[i] <- system.file("extdata/la_relation_keywords.csv", package="bibliographica")
    i <- i + 1
  }
  
  ret <- character(length=length(x))
  for (fil in f) {
    synonyms <- read.csv(file=fil, sep="\t", fileEncoding="UTF-8")
    for (i in 1:nrow(synonyms)) {
      if (is.null(full_name)) {
        pattern <- as.character(synonyms$synonyme[i])
        replacement <- as.character(synonyms$name[i])
      } else {
        pattern <- str_replace(synonyms$synonyme[i], "<name>", full_name)
        replacement <- as.character(synonyms$name[i])
        
      }
      if (length(x) != 0) {
        if (is.na(str_extract(x, pattern))) {
        } else {
          inds <- intersect(which(!is.na(str_extract(x, pattern))), which(ret==""))
          ret[inds] <- replacement
        }
      }

    }
  }
  ret
  
}