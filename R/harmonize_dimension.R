#' Harmonize dimension information 
#'
#' @param x A character vector that may contain dimension information
#' @param synonyms Synonyme table
#' @return The character vector with dimension information harmonized
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' 
#' @examples harmonize_dimension("fol.")
#' @keywords internal
harmonize_dimension <- function (x, synonyms = NULL) {

  if (is.null(synonyms)) {
    f <- system.file("extdata/harmonize_dimensions.csv", package = "bibliographica")
    synonyms <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
  } 

  s <- tolower(as.character(x))

  # "1/2."
  inds <- grep("^1/[0-9]\\.$", s)
  s[inds] <- gsub("\\.", "to", gsub("1/", "", s[inds]))

  # "1/2"
  inds <- grep("^1/[0-9]$", s)
  s[inds] <- gsub("1/", "", s[inds])

  # "8"
  inds <- grep("[0-9]$", s)
  s[inds] <- paste0(s[inds], "to")

  # 8.
  inds <- grep("^[0-9]+\\.$", s)
  s[inds] <- gsub("\\.$", "to", s[inds])

  for (i in 1:5) {
    s <- remove_endings(s, c(" ", "\\.", "\\,", "\\;", "\\:", "\\?"))
  }

  # "16mo in 8's."
  inds <- grep("[0-9]. in [0-9]'s", s)
  s[inds] <- gsub(" in [0-9]'s", "", s[inds])

  # Harmonize the terms
  s <- harmonize_names(s, synonyms, mode = "recursive")$name

  # "4; sm 2; 2" -> NA
  inds <- grep("[0-9]+.o; sm [0-9]+.o; [0-9]+.o", s)
  s[inds] <- gsub("[0-9]+.o; sm [0-9]+.o; [0-9]+.o", "", s[inds])
  
  # This could not be handled in harmonization csv for some reason
  s <- gsub("(fol)", "2fo", s)

  # Add spaces
  s <- gsub("cm\\.", " cm ", s)  
  s <- gsub("cm", " cm ", s)
  s <- gsub("x", " x ", s)
  s <- gsub("obl\\.", "obl ", s)
  s <- gsub("obl", "obl ", s)    

  # Remove extra spaces
  s <- condense_spaces(s)

  # 2fo(7)
  inds <- c(grep("[0-9].o\\([0-9]\\)$", s),
          grep("[0-9].o\\([0-9]\\?\\)$", s))
  s[inds] <- substr(s[inds], 1, 3)

  # cm12mo
  inds <- grep("^cm[0-9][0-9].o$", s)
  s[inds] <- substr(s[inds], 3, 6)

  # cm4to
  inds <- grep("^cm[0-9].o$", s)
  s[inds] <- substr(s[inds], 3, 5)

  # cm.12mo
  inds <- grep("^cm\\.[0-9][0-9].o$", s)
  s[inds] <- substr(s[inds], 4, 7)

  # cm.4to
  inds <- grep("^cm\\.[0-9].o$", s)
  s[inds] <- substr(s[inds], 4, 6)

  # "12mo.f"
  inds <- grep("[0-9].o\\.f$", s)
  s[inds] <- gsub(".f", "", s[inds])

  # "4to;2fo" -> "4to-2fo"
  inds <- grep("^[0-9].o;[0-9].o$", s)
  s[inds] <- gsub(";", "-", s[inds])

  #4to.;4to
  inds <- grep("^[0-9]+.o\\.;[0-9]+.o$", s)
  s[inds] <- gsub("\\.;", "-", s[inds])

  #4to;, 4to
  inds <- grep("[0-9]+.o;, [0-9]+.o", s)
  s[inds] <- gsub(";, ", "-", s[inds])

  #4to; 4to or 4to, 4to
  inds <- grep("[0-9]+.o(;|,) [0-9]+.o", s)
  s[inds] <- gsub("(;|,) ", "-", s[inds])

  #4to;4to
  inds <- grep("[0-9]+.o;[0-9]+.o", s)
  s[inds] <- gsub(";", "-", s[inds])

  #2; 1/2; 2
  inds <- grep("[0-9]+.o; [0-9]+.o; [0-9]+.o", s)
  s[inds] <- gsub(";", "-", s[inds])

  # 4to, 2fo and 1to
  inds <- grep("^[0-9]+.o, [0-9]+.o and [0-9]+.o$", s)
  s[inds] <- NA

  #4to and 8vo -> 4to-8vo
  inds <- grep("[0-9]+.o and [0-9]+.o", s)
  s[inds] <- gsub(" and ", "-", s[inds])  

  #4to, 8vo
  inds <- grep("^[0-9]+.o, [0-9]+.o", s)
  s[inds] <- gsub(", ", "-", s[inds])

  # 46cm(2fo) -> 46cm (2fo)
  s <- gsub("cm\\.*\\(", "cm (", s)

  s

}

