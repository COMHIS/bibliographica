#' @title Harmonize dimension
#' @description Harmonize dimension information 
#' @param x A character vector that may contain dimension information
#' @param synonyms Synonyme table
#' @return The character vector with dimension information harmonized
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @importFrom sorvi condense_spaces
#' @examples \dontrun{harmonize_dimension("fol.")}
#' @keywords internal
harmonize_dimension <- function (x, synonyms = NULL) {

  s <- tolower(as.character(x))

  # Remove brackets
  s <- gsub("\\(", " ", gsub("\\)", " ", s)) 
  s <- gsub("\\[", " ", gsub("\\]", " ", s)) 
  s <- condense_spaces(s)

  # Remove spaces
  s <- gsub(" \\:o", ":o", s)
  s <- gsub(" \\:0", ":0", s)  
  s <- gsub("[ |\\.|\\,|\\;|\\:|\\?]+$", "", s)

  # 1915
  s[grep("^[0-9]{3,8}$", s)] <- NA

  # 2.o
  inds <- grep("^[0-9]*\\.{0,1}o$", s)
  s[inds] <- gsub("\\.o", "to", s[inds])

  # 2:0
  inds <- grep("\\:0", s)
  s[inds] <- gsub("\\:0", "to", s[inds])

  # 12top
  inds <- grep("^[0-9]*top$", s)
  s[inds] <- gsub("p", "", s[inds])

  # "1/2"
  inds <- grep("^1/[0-9]*$", s)
  s[inds] <- gsub("1/", "", s[inds])

  # "8" & "20"
  inds <- grep("^[0-9]*?$", s)
  s[inds] <- paste0(s[inds], "to")

  # "16mo in 8's."
  inds <- grep("[0-9]. in [0-9]'s", s)
  s[inds] <- gsub(" in [0-9]'s", "", s[inds])

  # Harmonize the terms
  s <- harmonize_names(s, synonyms, mode = "recursive", check.synonymes = FALSE, include.lowercase = F)

  # "4; sm 2; 2" -> NA
  inds <- grep("[0-9]+.o; sm [0-9]+.o; [0-9]+.o", s)
  s[inds] <- gsub("[0-9]+.o; sm [0-9]+.o; [0-9]+.o", "", s[inds])
  
  # This could not be handled in harmonization csv for some reason
  s <- gsub("(fol)", "2fo", s)

  # Add spaces
  s <- gsub("cm\\. {0,1}", " cm ", s)  
  s <- gsub("x", " x ", s)
  s <- gsub("obl\\.{0,1}", "obl ", s)

  # Remove extra spaces
  s <- condense_spaces(s)

  # 2fo(7)
  inds <- c(grep("[0-9].o\\([0-9]\\)$", s),
          grep("[0-9].o\\([0-9]\\?\\)$", s))
  s[inds] <- substr(s[inds], 1, 3)

  # cm12mo
  inds <- grep("^cm[0-9]*.o$", s)
  s[inds] <- substr(s[inds], 3, 6)

  # cm.12mo
  inds <- grep("^cm\\.[0-9]*.o$", s)
  s[inds] <- substr(s[inds], 4, 7)

  # "12mo.f"
  inds <- grep("[0-9]*.o\\.[a-z]$", s)
  s[inds] <- gsub(".f", "", s[inds])

  # "4to;2fo" -> "4to-2fo"
  inds <- grep("^[0-9]+.o;[0-9]+.o$", s)
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

  # 4to and 8vo -> 4to-8vo
  inds <- grep("[0-9]+.o and [0-9]+.o", s)
  s[inds] <- gsub(" and ", "-", s[inds])  

  #4to, 8vo
  inds <- grep("^[0-9]+.o, [0-9]+.o", s)
  s[inds] <- gsub(", ", "-", s[inds])

  # 46cm(2fo) -> 46cm (2fo)
  s <- gsub("cm\\.*\\(", "cm (", s)

  s

}

