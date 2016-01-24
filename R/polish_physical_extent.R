#' @title Polish physical_extent field
#' @description Pick page counts, volume counts and volume numbers
#' @param x Page number field. Vector or factor of strings.
#' @param verbose Print progress info
#' @return Raw and estimated pages per document part
#' @details Document parts are separated by semicolons
#' @export
#' @importFrom stringr str_trim
#' @details A summary of page counting rules that this function aims to (approximately) implement are provided in 
#' \url{https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html}
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # polish_pages("4p.")
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
  s <- harmonize_names(s, page.synonyms, mode="match")$name
  
  s <- harmonize_ie(s)

  # Read the mapping table
  f <- system.file("extdata/harmonize_pages.csv", package = "bibliographica")
  page.harmonize <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))
  #s <- harmonize_names(s, page.harmonize, mode = "recursive")$name
  
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
  s <- harmonize_names(s, romans.harm, mode = "recursive")$name

  # Remove some rare misleading special cases manually
  s <- gsub("v.1-3, 5 ;", "", s)
  s <- gsub("v.1,4-7 ;", "", s)

  # ie harmonization
  # each comma place separately
  # each dash place separately  
  s <- gsub("e\\.\\,", "e ", s)

  # Polish unique pages separately for each volume
  # Return NA if conversion fails
  ret <- lapply(s, function (s) { a <- try(polish_physext_help(s, verbose = verbose, page.synonyms, page.harmonize, sheet.harmonize, harm.pi)); if (class(a) == "try-error") { return(NA) } else { return(a) }})

  # Convert to data.frame
  ret <- data.frame(do.call("rbind", ret))

  # Some final polishing
  ret$pagecount[ret$pagecount == 0] <- NA # Set zero page counts to NA

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

  if (verbose) {message(s)}
  if (is.na(s)) { return(NA) }
  
  # Estimate pages for each document separately via a for loop
  # Vectorization would be faster but we prefer simplicity and modularity here

  #' A single document, but check which volume number ?
  # (document starting with '* v.' or 'v.1-3' etc.)  
  # (document starting with 'v.*')
  voln <- pick_volume(s)

  # Volume count
  vols <- pick_multivolume(s)

  # Assume single volume when number not given
  # FIXME perhaps this better goes to enrichnment functions?
  # NOTE: voln (volume number must be NA as well, otherwise we have 
  # one part of a multi-volume document
  vols[is.na(vols) & is.na(voln)] <- 1 

  # Pagecount
  spl <- unlist(strsplit(s, ";"))
  x <- try(unname(sapply(spl, function (x) {polish_physext_help2(x, vols, page.synonyms, page.harmonize, sheet.harmonize, harm.pi)})))
  if (class(x) == "try-error") {
    x <- NA
  } 

  # Return
  data.frame(pagecount = sum(x, na.rm = TRUE), volnumber = voln, volcount = vols)

}



#' @title Polish physical_extent help field 2
#' @description Internal
#' @param s Input char
#' @param vols vols
#' @param page.synonyms page.synonyms
#' @return Internal
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # TBA
#' @keywords internal
polish_physext_help2 <- function (s, vols, page.synonyms, page.harmonize, sheet.harmonize, harm.pi) {

  x <- suppressWarnings(remove_volume_info(s, vols))

  x <- harmonize_pages(x, page.synonyms, page.harmonize, sheet.harmonize, harm.pi) 

  x <- estimate_pages(x)

  x
  
}




