#' @title Remove Print Statements
#' @description Remove print statements.
#' @param x a vector
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- remove_print_statements("Printed in London")
#' @keywords utilities
remove_print_statements <- function (x) {

  x0 = xorig <- tolower(as.character(x))
  xuniq <- unique(xorig)
  x <- xuniq

  terms.single = c()
  terms.multi = c()  

  ### Get printing terms from tables in various languages
  for (lang in c("finnish", "french", "german", "swedish", "english")) {

    f <- system.file(paste0("extdata/printstop_", lang, ".csv"), package = "bibliographica")
    terms <- unique(str_trim(tolower(read.csv(f, stringsAsFactors = FALSE)[,1])))

    # Harmonize the terms 
    terms.multi <- c(terms.multi, terms[nchar(terms) > 1])
    terms.single <- c(terms.single, terms[nchar(terms) == 1])

  }

  terms.multi = unique(terms.multi)
  terms.single = unique(terms.single)  

  x <- remove_terms(x, terms.multi, where = "all", polish = FALSE, include.lowercase = FALSE)
  x <- condense_spaces(x)

  # Back to original indices, then unique again; reduces
  # number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)

  # Individual characters not removed from the end
  x <- remove_terms(x, terms.single, where = "begin", polish = FALSE, include.lowercase = FALSE)

  # Back to original indices, then unique again; reduces
  # number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)

  x <- remove_terms(x, terms.single, where = "middle", polish = FALSE, include.lowercase = FALSE)
  x <- condense_spaces(x)
  x <- remove_trailing_periods(x)

  # Back to original indices, then unique again; reduces
  # number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)

  # remove sine loco
  f <- system.file("extdata/sl.csv", package = "bibliographica") 
  sineloco <- as.character(read.csv(f)[,1])
  x <- remove_terms(x, sineloco, include.lowercase = TRUE)

  # Back to original indices, then unique again; reduces
  # number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)

  # handle some odd cases manually
  # FIXME: estc-specific, move there
  # "122 s"; "2 p"
  x[grep("^[0-9]* [s|p]$", x)] <- NA
  x <- gsub("[0-9]\\. p\\.;","",x)
  x <- gsub("^(.*?);.*$","\\1",x) # nb. non-greedy match
  x[x==""] <- NA

  x = x[match(xorig, xuniq)]
  
  x
}
