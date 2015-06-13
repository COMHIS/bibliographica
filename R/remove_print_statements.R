#' @title remove_print_statements
#' @description Remove print statements
#'
#' @param x a vector
#' @return Polished vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples x2 <- remove_print_statements("Printed in London")
#' @keywords utilities
remove_print_statements <- function (x) {

  # Get printing terms from a table
  f <- system.file("extdata/printterms.csv", package = "bibliographica")
  terms <- as.character(read.csv(f)[,1])

  # Add versions with endings
  terms <- c(terms, 
  	     paste(terms, " at", sep = ""),
  	     paste(terms, " in", sep = ""),
	     paste(terms, " i", sep = ""),
	     paste(terms, " j", sep = ""),
	     paste(terms, " på", sep = ""),
	     paste(terms, " uti", sep = ""),
	     paste(terms, " uthi", sep = ""),
	     paste(terms, " zu", sep = ""),
	     paste(terms, " a", sep = ""),
	     paste("a ", terms, sep = ""),
	     paste("A ", terms, sep = ""),
	     paste("I ", terms, sep = "")
	     )

  # Remove printing terms
  x <- remove_terms(x, terms, include.lowercase = TRUE)

  # sine loco
  x[x=="Sl"] <- NA
  x[x=="S l"] <- NA
  x[x=="Sl "] <- NA
  x[x=="sl"] <- NA
  x[x=="s l"] <- NA
  x[x=="Sn"] <- NA
  x[x=="sn"] <- NA
  x[x=="Sa"] <- NA
  x[x=="sa"] <- NA
  x[x=="SI"] <- NA
  x[x==""] <- NA

  # odd cases
  x[x=="122 s"] <- NA
  x[x=="204 s"] <- NA
  x[x=="2 p"] <- NA

  # semicolons (note: this removes a few dozen city names)
  x <- gsub("2. p.;","",x)
  x <- gsub("S.l.;","",x)
  x <- gsub("^(.*?);.*$","\\1",x) # nb. non-greedy match

  x
}
