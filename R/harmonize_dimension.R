#' @title Harmonize dimension
#' @description Harmonize dimension information 
#' @param x A character vector that may contain dimension information
#' @param synonyms Synonyme table
#' @return The character vector with dimension information harmonized
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{harmonize_dimension("fol.")}
#' @keywords internal
harmonize_dimension <- function (x, synonyms = NULL) {

  s <- x
  s <- gsub("[ |\\.|\\,|\\;|\\:|\\?]+$", "", s)

  # 1915
  s[grep("^[0-9]{4,}$", s)] <- NA

  # "8" & "20"
  inds <- grep("^[0-9]*?$", s)
  s[inds] <- paste0(s[inds], "to")

  # "4; sm 2; 2" -> NA
  inds <- grep("[0-9]+.o; sm [0-9]+.o; [0-9]+.o", s)
  s[inds] <- gsub("[0-9]+.o; sm [0-9]+.o; [0-9]+.o", "", s[inds])

  # 2fo(7)
  inds <- c(grep("[0-9].o\\([0-9]\\)$", s),
          grep("[0-9].o\\([0-9]\\?\\)$", s))
  s[inds] <- substr(s[inds], 1, 3)

  # cm12mo cm.12mo
  inds <- grep("^cm\\.{0,1}[0-9]+.o$", s)
  s[inds] <- substr(s[inds], 3, 6)

  #"49 cm 2fo, 2fo"
  inds <- grep("^[0-9]* cm [0-9]+.o\\, [0-9]+.o$", s)
  s[inds] <- gsub("\\, ", "-", s[inds])

  #2; 1/2; 2
  inds <- grep("[0-9]+.o; [0-9]+.o; [0-9]+.o", s)
  s[inds] <- gsub(";", "-", s[inds])

  # 4to, 2fo and 1to
  inds <- grep("^[0-9]+.o, [0-9]+.o and [0-9]+.o$", s)
  s[inds] <- NA

  # 4to and 8vo -> 4to-8vo
  inds <- grep("[0-9]+.o and [0-9]+.o", s)
  s[inds] <- gsub(" and ", "-", s[inds])  

  #4to.;4to
  inds <- grep("^[0-9]+.o[\\.|\\,|;| ]+[0-9]+.o$", s)
  s[inds] <- gsub("[\\.|\\,|;| ]+", "-", s[inds])

  s <- gsub("- ", "-", s)

  s

}

