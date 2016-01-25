#' @title Polish physical_extent field
#' @description Pick page counts, volume counts and volume numbers
#' @param x Page number field. Vector or factor of strings.
#' @param verbose Print progress info
#' @return Raw and estimated pages per document part
#' @details Document parts are separated by semicolons
#' @export
#' @details A summary of page counting rules that this function aims to (approximately) implement are provided in 
#' \url{https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html}
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples tab <- polish_physical_extent("4p.")
#' @keywords utilities
polish_physical_extent <- function (x, verbose = FALSE) {

  # Summary of abbreviations
  # http://ac.bslw.com/community/wiki/index.php5/RDA_4.5
  sorig <- as.character(x)
  suniq <- unique(sorig)

  if (verbose) {
    message(paste("Polishing page count field:", length(suniq), "unique cases"))
  }

  #------------------------------------------------------

  s <- suniq

  # Remove commonly used volume formats
  f <- system.file("extdata/remove_dimension.csv", package = "bibliographica")
  terms <- as.character(read.csv(f)[,1])
  s <- remove_dimension(s, terms)

  s <- as.character(s)
  s[grep("^[ |\\.|;|:|!|?]*$", s)] <- NA # ""; "." ; " ... "

  # Harmonize volume info
  inds <- 1:length(s)  
  inds <- setdiff(inds, setdiff(grep("v\\.$", s), grep("^v\\.$", s)))
  if (length(inds)>0) {
    s[inds] <- remove_trailing_periods(s[inds])
  }
  s <- unname(harmonize_volume(s))

  # In Finnish texts s. is used instead of p.		
  f <- system.file("extdata/translation_fi_en_pages.csv", package = "bibliographica")
  page.synonyms <- read.csv(f, sep = ";")
  s <- harmonize_names(s, page.synonyms, mode="match", include.lowercase = FALSE, check.synonymes = F)
  
  s <- harmonize_ie(s)

  # Read the mapping table
  f <- system.file("extdata/harmonize_pages.csv", package = "bibliographica")
  page.harmonize <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))
  
  # Pp. -> p etc.
  f <- system.file("extdata/harmonize_page_info.csv", package = "bibliographica")
  harm.pi <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))  

  # Read the mapping table
  f <- system.file("extdata/harmonize_sheets.csv", package = "bibliographica")
  sheet.harmonize <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))
  s <- harmonize_sheets(s, sheet.harmonize)

  # Read the mapping table
  f <- system.file("extdata/harmonize_romans.csv", package = "bibliographica")
  romans.harm <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))
  # Romans
  s <- harmonize_names(s, romans.harm, mode = "recursive", include.lowercase = FALSE, check.synonymes = F)

  # Remove some rare misleading special cases manually
  s <- gsub("a-m", " ", s)

  # ie harmonization
  # each comma place separately
  # each dash place separately  
  s <- gsub("e\\.\\,", "e ", s)

  # Polish unique pages separately for each volume
  # Return NA if conversion fails

  pages <- sapply(s, function (s) { a <- try(polish_physext_help(s, verbose = verbose, page.synonyms, page.harmonize, sheet.harmonize, harm.pi)); if (class(a) == "try-error") { return(NA) } else { return(a) }})
  rownames(pages) <- NULL

  # Make data frame
  ret <- data.frame(unname(t(pages)))
  for (k in 1:ncol(ret)) {ret[, k] <- unlist(ret[, k], use.names = FALSE)}
  names(ret) <- c("pagecount", "volnumber", "volcount")

  # Assume single volume when number not given
  # FIXME perhaps this better goes to enrichnment functions?
  # NOTE: voln (volume number must be NA as well, otherwise we have 
  # one part of a multi-volume document
  ret$volcount[is.na(ret$volcount) & is.na(ret$volnumber)] <- 1 

  # Some final polishing
  ret$pagecount[is.na(ret$pagecount) | ret$pagecount == 0] <- NA # Set zero page counts to NA

  if (verbose) { message("Project unique entries back to the original list") }
  ret[match(sorig, suniq), ]

}




#' @title Polish physical_extent help field
#' @description Internal
#' @param s Input char
#' @param verbose Print progress info
#' @return Internal
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # TBA
#' @keywords internal
polish_physext_help <- function (s, verbose, page.synonyms, page.harmonize, sheet.harmonize, harm.pi) {

  if (verbose) {message(paste(s, "\n"))}
  if (is.na(s)) { return(NA) }

  # Shortcut for easy cases: "24p."
  if (length(grep("[0-9]+ {0,1}p\\.{0,1}$",s))>0) {
    c(as.numeric(str_trim(gsub(" {0,1}p\\.{0,1}$", "", s))), NA, NA)  
  }

  # Pick volume number
  voln <- pick_volume(s) 

  # Volume count
  vols <- pick_multivolume(s)

  # Now remove volume info
  s <- suppressWarnings(remove_volume_info(s))

  # Estimate pages for each document separately via a for loop
  # Vectorization would be faster but we prefer simplicity and modularity here

  # Pagecount
  spl <- unlist(strsplit(s, ";"), use.names = FALSE)
  x <- try(unname(sapply(spl, function (x) {polish_physext_help2(x, page.synonyms, page.harmonize, sheet.harmonize, harm.pi)})))
  if (class(x) == "try-error") {
    x <- NA
  } 

  # Return
  #c(pagecount = sum(x, na.rm = TRUE), volnumber = voln, volcount = vols)
  c(sum(x, na.rm = TRUE), voln, vols)  

}



#' @title Polish physical_extent help field 2
#' @description Internal
#' @param x Input char
#' @param page.synonyms page.synonyms
#' @return Internal
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # TBA
#' @keywords internal
polish_physext_help2 <- function (x, page.synonyms, page.harmonize, sheet.harmonize, harm.pi) {

  x <- harmonize_pages(x, page.synonyms, page.harmonize, sheet.harmonize, harm.pi) 

  x <- estimate_pages(x)

  x
  
}




