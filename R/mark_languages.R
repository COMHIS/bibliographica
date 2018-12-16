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

  x0 <- x

  # Harmonize
  x <- tolower(as.character(x))	       
  x[x == "NA"] <- ""
  x[x == "n/a"] <- ""
  x[is.na(x)] <- ""  
  x <- gsub("^;","",x)
  x <- gsub(";$","",x)
  x <- condense_spaces(x)

  # Unique entries only to speed up
  xorig <- x
  xuniq <- unique(xorig)
  x <- xuniq

  # Convert to polished language names as in
  # http://www.loc.gov/marc/languages/language_code.html
  # TODO: XML version available, read directly in R:
  # see http://www.loc.gov/marc/languages/
  f <- system.file("extdata/language_abbreviations.csv", package = "bibliographica")
  abrv <- read.csv(f, sep = "\t", header = TRUE, encoding = "UTF-8")

  # Further harmonization
  x <- gsub("\\(", " ", x)
  x <- gsub("\\)", " ", x)
  x <- gsub("\\,", " ", x)
  x <- gsub(" +", " ", x)
  x <- condense_spaces(x)
  # Final name
  #abrv$name <- gsub("\\(", " ", abrv$name)
  #abrv$name <- gsub("\\)", " ", abrv$name)
  #abrv$name <- gsub("\\,", " ", abrv$name)
  #abrv$name <- gsub(" +", " ", abrv$name)    
  abrv$synonyme <- gsub("\\(", " ", abrv$synonyme)
  abrv$synonyme <- gsub("\\)", " ", abrv$synonyme)
  abrv$synonyme <- gsub("\\,", " ", abrv$synonyme)
  abrv$synonyme <- gsub(" +", " ", abrv$synonyme)  
  abrv <- unique(abrv)

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
      
      # Just unique languages
      # "Undetermined;English;Latin;Undetermined"
      # -> "Undetermined;English;Latin"
      lll <- unique(lll)
      x[[i]] <- paste(lll, collapse = ";")

  }
  
  # List all unique languages in the data
  x[x %in% c("NA", "Undetermined", "und")] <- NA
  xu <- na.omit(unique(unname(unlist(strsplit(unique(x), ";")))))

  # Only accept the official / custom abbreviations
  # (more can be added on custom list if needed)
  xu <- intersect(xu, abrv$name)

  len <- sapply(strsplit(x, ";"), length)
  dff <- data.frame(language_count = len)  
  
  multilingual <- len > 1
  dff$multilingual <- multilingual

  # Now check just the unique and accepted ones, and collapse
  # TODO: add ID for those that are not recognized
  # NOTE: the language count and multilingual fields should be fine however
  # as they are defined above already
  x <- sapply(strsplit(x, ";"), function (xi) {paste(unique(intersect(xi, xu)), collapse = ";")})

  dff$languages <- x
  inds <- which(dff$languages == "")
  if (length(inds) > 0) {
    dff$languages[inds] <- "Undetermined"
  }

  if (length(grep(";", dff$languages)) > 0) {
    dff$language_primary <- sapply(strsplit(dff$languages, ";"),
                                               function (x) {x[[1]]})
  } else {
    dff$language_primary <- dff$languages
  }

  # Convert to factors
  dff$languages <- as.factor(str_trim(dff$languages))
  dff$language_primary <- as.factor(str_trim(dff$language_primary))
  
  dff[match(xorig, xuniq),]

}
