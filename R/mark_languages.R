#' @title Mark Languages
#' @description Construct binary matrix of languages for each entry
#' @param x language field (a vector)
#' @return data.frame with separate fields for different languages
#' @export
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- mark_languages(c("fin;lat","eng"))}
#' @keywords utilities
mark_languages <- function(x) {

  # Harmonize
  x <- tolower(as.character(x))	       
  x[x == "NA"] <- ""
  x[x == "n/a"] <- ""
  x[is.na(x)] <- ""  
  x <- gsub("^;","",x)
  x <- gsub(";$","",x)

  # Unique entries only to speed up
  xorig <- x
  xuniq <- unique(xorig)
  x <- xuniq

  # Convert to polished language names as in
  # http://www.loc.gov/marc/languages/language_code.html
  # TODO: XML version available, read directly in R:
  # see http://www.loc.gov/marc/languages/
  f <- system.file("extdata/language_abbreviations.csv", package = "bibliographica")
  abrv <- read_mapping(f,
	include.lowercase = TRUE, # some catalogs may use uppercase, others not
       	self.match = TRUE, # some catalogs may use abbreviations, others not
	ignore.empty = FALSE,
       	mode = "table", sep = "\t")

  # Unrecognized languages?
  unrec <- as.vector(na.omit(setdiff(
  	     unique(unlist(strsplit(as.character(unique(x)), ";"))),
	     abrv$synonyme
	     )))
  
  if (length(unrec) > 0) {
    warning(paste("Unidentified languages: ", paste(unrec, collapse = ";")))
  }

  # TODO Vectorize to speed up ?
  for (i in 1:length(x)) {
    
      lll <- sapply(unlist(strsplit(x[[i]], ";")), function (xx) {
               as.character(map(xx, abrv, remove.unknown = TRUE, mode = "exact.match"))
	       })

      lll <- na.omit(as.character(unname(lll)))
      if (length(lll) == 0) {lll <- NA}

      x[[i]] <- paste(lll, collapse = ";")

  }
  
  # List all unique languages in the data
  x[x %in% c("NA", "Undetermined", "und")] = NA
  xu <- na.omit(unique(unname(unlist(strsplit(unique(x), ";")))))

  # Only accept the official / custom abbreviations
  # (more can be added on custom list if needed)
  xu <- intersect(xu, abrv$name)

  # Provide logical vectors for the language hits for
  # each accepted language
  subroutine <- function(abbrv, x){
     grepl(paste("^", abbrv, "$", sep = ""), x, ignore.case = T) |
     grepl(paste("^", abbrv, sep = ""), x, ignore.case = T) |
     grepl(paste(";", abbrv, sep = ""), x, ignore.case = T) |
     grepl(paste(";", abbrv, "$", sep = ""), x, ignore.case = T) 
  }
  li <- list()
  for (u in setdiff(xu, "Multiple languages")) {
    li[[u]] <- subroutine(u, x)
  }
  u <- "Multiple languages"
  li[[u]] <- subroutine(u, x) | grepl(";", x)
  
  dff <- as_data_frame(li)
  names(dff) <- paste("language.", names(dff), sep = "")
  
  dff$language <- as.factor(x)
  
  dff[match(xorig, xuniq),]

}
