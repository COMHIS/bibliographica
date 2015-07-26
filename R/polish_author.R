#' @title polish_author
#' @description Polish author
#'
#' @param s Vector of author names
#' @param stopwords Stopwords
#' @return Polished vector
#'
#' @export
#' @importFrom tm stopwords
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples s2 <- polish_author("Smith, William")
#' @keywords utilities
polish_author <- function (s, stopwords = NULL) {

  s <- as.character(s)

  # Remove brackets and ending commas / periods
  s <- remove_numerics(s)
  s <- gsub("\\[", "", s)
  s <- gsub("\\]", "", s)
  s <- gsub("\\(", "", s)
  s <- gsub("\\)", "", s)
  s <- str_trim(s)
  s <- gsub("\\.$", "", s)
  s <- gsub("\\,$", "", s)

  # Assume names are of format Last, First
  nametab <- t(sapply(strsplit(s, ","), function (x) {
    name <- c(last = x[[1]], first = NA);
    if (length(x)>1) {name[["first"]] <- paste(x[-1], collapse = " ")};
    return(name)
  }))

  if (is.null(stopwords)) {
    message("No stopwords provided for authors. Using ready-made stopword lists")
    f <- system.file("extdata/stopwords.csv", package = "bibliographica")
    stopwords.general <- as.character(read.csv(f, sep = "\t")[,1])
    stopwords.general <- c(stopwords.general, stopwords(kind = "en"))
    f <- system.file("extdata/stopwords_for_names.csv", package = "bibliographica")
    stopwords.names <- as.character(read.csv(f, sep = "\t")[,1])
    f <- system.file("extdata/stopwords_titles.csv", package = "bibliographica")
    stopwords.titles <- as.character(read.csv(f, sep = "\t")[,1])
    stopwords <- unique(c(stopwords.general, stopwords.names, stopwords.titles))
  }

  # Harmonize names
  nametab <- as.data.frame(nametab)
  nametab$last  <- gsub("^-", "", trim_names(nametab$last,  stopwords, remove.letters = TRUE))
  nametab$first <- gsub("^-", "", trim_names(nametab$first, stopwords, remove.letters = FALSE))

  # Some additional formatting
  # "Wellesley, Richard Wellesley" -> "Wellesley, Richard"
  firsts <- c()
  for (i in 1:nrow(nametab)) {
    x <- nametab[i,]
    first <- unlist(strsplit(nametab[i, "first"], " "))
    last <- unlist(strsplit(nametab[i, "last"], " "))
    if (!is.na(first) && !is.na(last)) {
      if (last == first[[length(first)]]) {
        first <- first[-length(first)]
      }
    }
    firsts[[i]] <- paste(first, collapse = " ")
  }
  nametab$first <- firsts

  nametab

}

