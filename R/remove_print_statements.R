#' @title remove_print_statements
#' @description Remove print statements
#' @param x a vector
#' @param remove.letters Remove individual letters TRUE/FALSE
#' @param n.iter Number of iterative repetitions of this function
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- remove_print_statements("Printed in London")
#' @keywords utilities
remove_print_statements <- function (x, remove.letters = FALSE, n.iter = 1) {

  x <- xorig <- as.character(x)
  x <- tolower(x)

  ### Get printing terms from tables in various languages
  for (lang in c("finnish", "french", "german", "swedish", "english")) {
    f <- system.file(paste0("extdata/printstop_", lang, ".csv"), package = "bibliographica")
    terms <- read.csv(f, stringsAsFactors = FALSE)[,1]

    # Harmonize the terms 
    terms.multi <- terms[nchar(terms) > 1]
    x <- remove_terms(x, terms.multi, where = "all", polish = TRUE, include.lowercase = TRUE)
    
    # Individual characters not removed from the end
    terms.single <- terms[nchar(terms) == 1]    
    x <- remove_terms(x, terms.single, where = "begin", polish = TRUE, include.lowercase = TRUE)
    x <- remove_terms(x, terms.single, where = "middle", polish = TRUE, include.lowercase = TRUE)

    if (remove.letters) {
      x <- remove_letters(x)
    }

  }

  # remove sine loco
  x <- remove_sl(x)

  # handle some odd cases manually
  # FIXME: this is estc-specific, move there
  # "122 s"; "2 p"
  x[grep("^[0-9]* [s|p]$", x)] <- NA
  x <- gsub("2\\. p\\.;","",x)
  x <- gsub("^(.*?);.*$","\\1",x) # nb. non-greedy match
  x[x==""] <- NA

  # Repeat n.iter times
  if (n.iter > 1) {
    for (cnt in 1:n.iter) {
      # message(paste("remove_print_statements", cnt))
      x <- remove_print_statements(x, remove.letters = remove.letters, n.iter = 0)
    }
  }

  x

}
