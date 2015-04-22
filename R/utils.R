#' @title remove_endings
#' @description Remove specified endings of strings
#'
#' @param x vector
#' @param endings endings to remove
#' @return polished vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- remove_endings(x, endings)}
#' @keywords utilities
remove_endings <- function (x, endings) {

  for (e in endings) {
    x <- gsub(paste(e, "$", sep = ""), "", x)
  }

  x
}


#' @title write_xtable
#' @description Write xtable in a file
#'
#' @param x a vector 
#' @param filename output file 
#' @return Table indicating the count for each unique entry in the input 
#'         vector. As a side effect the function writes this in the file.
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{tab <- write_xtable(x, "tmp.tab")}
#' @keywords utilities
write_xtable <- function (x, filename) {

  if (is.vector(x)) {	     

    counts <- rev(sort(table(x)))
    tab <- data.frame(list(Name = names(counts), Count = counts))

  } else if (is.matrix(x)) {

    id <- apply(tab, 1, function (x) {paste(x, collapse = "-")})
    ido <- rev(sort(table(id)))
    idn <- ido[match(id, names(ido))]
    tab <- cbind(tab, count = idn)
    tab <- tab[rev(order(as.numeric(tab[, "count"]))),]
    tab <- tab[!duplicated(tab),]

  }

  message(paste("Writing", filename))
  write.table(tab, file = filename, quote = FALSE, sep = "\t", row.names = FALSE)

  tab

}



#' @title remove_numerics
#' @description Remove numeric characters from the input strings
#'
#' @param x A vector
#' @param numbers numeric characters to be removed (all by default)
#' @return A vector with characters removed
#'
#' @details After removing the numerics, beginning, double and ending 
#'          spaces are also removed from the strings.
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- remove_numerics(x, numbers = 0:9)}
#' @keywords utilities
remove_numerics <- function (x, numbers = 0:9) {

  for (num in numbers) {
    x <- gsub(num, " ", x)
  }

  x <- condense_spaces(x)

  x
}

#' @title remove_terms
#' @description Remove the given terms from the strings
#'
#' @param x A vector
#' @param terms Terms to be removed
#' @param where Locations to be removed ("all" / "begin" / "middle" / "end")
#'
#' @return Vector with terms removed
#'
#' @details After removing the numerics, beginning, double and ending 
#'          spaces are also removed from the strings.
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- remove_terms(x, terms, where = "all")}
#' @keywords utilities
remove_terms <- function (x, terms, where = "all") {

  # "Beginning ", 
  # " middle ", 
  # " middle. ", 
  # " last$" 
  for (term in terms) {

    x <- gsub(paste("^", term, "$", sep = ""), " ", x)

    if (where %in% c("all", "begin")) {
      x <- gsub(paste("^", term, "$", sep = ""), " ", x)
      x <- gsub(paste("^", term, " ", sep = ""), " ", x)
      x <- gsub(paste("^", term, "\\. ", sep = ""), " ", x)
      x <- gsub(paste("^", term, "\\, ", sep = ""), " ", x)
    }

    if (where %in% c("all", "middle")) {
      x <- gsub(paste(" ", term, " ", sep = ""), " ", x)
      x <- gsub(paste(" ", term, "\\.", sep = ""), " ", x)
      x <- gsub(paste(" ", term, "\\,", sep = ""), " ", x)
    }

    if (where %in% c("all", "end")) {
      x <- gsub(paste(" ", term, "$", sep = ""), " ", x)
      x <- gsub(paste(" ", term, "\\.$", sep = ""), " ", x)
      x <- gsub(paste(" ", term, "\\,$", sep = ""), " ", x)
    }
  }

  x <- condense_spaces(x)

  x 

}


