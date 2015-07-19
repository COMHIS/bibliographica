#' Harmonize dimension information 
#'
#' @param x A character vector that may contain dimension information
#' @return The character vector with dimension information harmonized
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' 
#' @examples harmonize_dimension("fol.")
#' @keywords internal
harmonize_dimension <- function (x) {

  s <- tolower(as.character(x))

  # 8.
  inds <- grep("^[0-9]+\\.$", s)
  s[inds] <- gsub("\\.$", "to", s[inds])

  for (i in 1:5) {
    s <- remove_endings(s, c(" ", "\\.", "\\,", "\\;", "\\:", "\\?"))
  }

  # Harmonize the terms
  f <- system.file("extdata/harmonize_dimensions.csv", package = "bibliographica")
  sn <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))
  s <- harmonize_names(s, sn, mode = "recursive")$name

  # Add spaces
  s <- gsub("cm", " cm", s)
  s <- gsub("x", " x ", s)

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

  # "4to;2fo" "2fo;1to" "4to-2fo"
  inds <- grep("^[0-9].o(;|-)[0-9].o$", s)
  s[inds] <- NA

  #4to.;4to
  inds <- grep("^[0-9]+.o\\.;[0-9]+.o$", s)
  s[inds] <- gsub("\\.;", "-", s[inds])

  #4to;, 4to
  inds <- grep("^[0-9]+.o;, [0-9]+.o$", s)
  s[inds] <- gsub(";, ", "-", s[inds])

  #4to, 8vo
  inds <- grep("^[0-9]+.o, [0-9]+.o$", s)
  s[inds] <- gsub(", ", "-", s[inds])
  
  # 4to-4to
  inds <- grep("^[0-9]+.o-[0-9]+.o$", s)
  if (length(inds) > 0) {
    li <- sapply(s[inds], function (x) {unique(unlist(strsplit(x, "-")))})
    inds2 <- which(sapply(li, length) == 1)
    s[inds[inds2]] <- unlist(li[inds2])
  }

  inds <- grep("^[0-9]+.o, [0-9]+.o$", s)
  s[inds] <- gsub("\\.;", "-", s[inds])
  
  s

}

