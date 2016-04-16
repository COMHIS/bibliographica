#' @title Mark languages
#' @description Construct binary matrix of languages for each entry
#' @param x language field (a vector)
#' @return data.frame with separate fields for different languages
#' @export
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- mark_languages(c("fin;lat","eng"))}
#' @keywords utilities
mark_languages <- function(x) {

  x[x == "NA"] = ""
  x <- gsub("^;","",x)
  x <- gsub(";$","",x)

  # Unique entries only to speed up
  xorig <- x
  xuniq <- unique(xorig)
  x <- xorig

  # Convert to polished language names
  f <- system.file("extdata/language_abbreviations.csv", package = "bibliographica")
  abrv <- read_synonymes(f, include.lowercase = T, self.match = T, ignore.empty = FALSE, mode = "table")

  inds <- grep(";", x)
  if (length(inds)>0) {
    for (i in inds) {
    print(i)
      x[[i]] <- paste(sapply(unlist(strsplit(x[[i]], ";")), function (xx) {as.character(harmonize_names(xx, abrv, remove.unknown = FALSE, mode = "exact.match"))}), collapse = ";")
    }
  }
  inds <- setdiff(1:length(x), grep(";", x))
  x[inds] <- as.character(harmonize_names(x[inds], abrv, remove.unknown = FALSE, mode = "exact.match"))

  # List all unique languages in the data  	    
  xu <- na.omit(unique(unname(unlist(strsplit(unique(x), ";")))))

  # Provide logical vectors for the language hits for each language
  subroutine <- function(abbrv){grepl(abbrv, x, ignore.case = T)}
  li <- list()
  for (u in setdiff(xu, "mul")) {
    li[[u]] <- subroutine(u)
  }
  u <- "mul"
  li[[u]] <- subroutine(u) | (sapply(strsplit(x, ";"), function (x) {length(unique(x))}) > 1)

  dff <- as_data_frame(li)
  dff$language <- as.factor(x)
  
  dff[match(xorig, xuniq),]
  
}
