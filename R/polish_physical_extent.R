#' @title Polish physical_extent field
#' @description Pick page counts, volume counts and volume numbers
#' @param x Page number field. Vector or factor of strings.
#' @param verbose Print progress info
#' @param mc.cores Number of cores for parallelization
#' @return Raw and estimated pages per document part
#' @details Document parts are separated by semicolons
#' @export
#' @details A summary of page counting rules that this function aims to (approximately) implement are provided in 
#' \url{https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html}
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples tab <- polish_physical_extent("4p.")
#' @keywords utilities
polish_physical_extent <- function (x, verbose = FALSE, mc.cores = 1) {

  # Summary of abbreviations
  # http://ac.bslw.com/community/wiki/index.php5/RDA_4.5
  sorig <- tolower(as.character(x))
  suniq <- unique(sorig)

  if (verbose) {
    message(paste("Polishing physical extent field:", length(suniq), "unique cases"))
  }

  #------------------------------------------------------

  s <- suniq

  if (verbose) {message("Remove commonly used volume formats")}
  f <- system.file("extdata/remove_dimension.csv", package = "bibliographica")
  terms <- as.character(read.csv(f)[,1])
  s <- remove_dimension(s, terms)
  s[grep("^[ |;|:|!|?]*$", s)] <- NA 

  # Back to original indices and new unique reduction 
  s <- s[match(sorig, suniq)]
  sorig <- s
  suniq <- unique(sorig)
  s <- suniq

  if (verbose) {
    message(paste("Polishing physical extent field 2:", length(suniq), "unique cases"))
  }

  if (verbose) {message("Harmonize volume info")}
  inds <- 1:length(s)  
  inds <- setdiff(inds, setdiff(grep("v\\.$", s), grep("^v\\.$", s)))
  if (length(inds)>0) {
    s[inds] <- remove_trailing_periods(s[inds])
  }
  s <- unname(harmonize_volume(s))

  # In Finnish texts s. is used instead of p.		
  f <- system.file("extdata/translation_fi_en_pages.csv", package = "bibliographica")
  page.synonyms <- read_synonymes(f, sep = ";", mode = "table")
  s <- harmonize_names(s, page.synonyms, mode="match")

  # Back to original indices and new unique reduction 
  s <- s[match(sorig, suniq)]
  sorig <- s
  suniq <- unique(sorig)
  s <- suniq

  s <- harmonize_ie(s)

  if (verbose) {message("Read the mapping table for pages")}
  f <- system.file("extdata/harmonize_pages.csv", package = "bibliographica")
  page.harmonize <- read_synonymes(f, sep = "\t", mode = "table")

  # Pp. -> p etc.
  f <- system.file("extdata/harmonize_page_info.csv", package = "bibliographica")
  harm.pi <- read_synonymes(f, sep = "\t", mode = "table", remove.ambiguous = FALSE)

  if (verbose) {message("Read the mapping table for sheets")}  
  f <- system.file("extdata/harmonize_sheets.csv", package = "bibliographica")
  sheet.harmonize <- read_synonymes(f, sep = "\t", mode = "table")
  s <- harmonize_sheets(s, sheet.harmonize)

  # Back to original indices and new unique reduction 
  s <- s[match(sorig, suniq)]
  sorig <- s
  suniq <- unique(sorig)
  s <- suniq

  if (verbose) {message("Read the mapping table for romans")}  
  f <- system.file("extdata/harmonize_romans.csv", package = "bibliographica")
  romans.harm <- read_synonymes(f, sep = "\t", mode = "table")
  s <- harmonize_names(s, romans.harm, mode = "recursive")

  if (verbose) {message("Page harmonization part 2")}  
  f <- system.file("extdata/harmonize_pages2.csv", package = "bibliographica")
  harm2 <- read_synonymes(f, sep = "\t", mode = "table")
  s <- harmonize_names(s, harm2, mode = "recursive")

  if (verbose) {message("Polish unique pages separately for each volume")}  

  # Back to original indices and new unique reduction 
  s <- s[match(sorig, suniq)]
  sorig <- s
  suniq <- unique(sorig)

  if (verbose) {
    message(paste("Polishing physical extent field 3:", length(suniq), "unique cases"))
  }

  # Return NA if conversion fails
  pages <- parallel::mclapply(str_trim(suniq), function (s) { a <- try(polish_physext_help(s, verbose = verbose, page.synonyms, page.harmonize, sheet.harmonize, harm.pi)); if (class(a) == "try-error") {return(NA)} else {return(a)}}, mc.cores = mc.cores)
  pages <- t(sapply(pages, identity))

  if (verbose) {message("Make data frame")}  
  ret <- as_data_frame(pages)
  names(ret) <- c("pagecount", "volnumber", "volcount")

  # Assume single volume when number not given
  # FIXME perhaps this better goes to enrichnment functions?
  # NOTE: voln (volume number must be NA as well, otherwise we have 
  # one part of a multi-volume document
  ret$volcount[is.na(ret$volcount) & is.na(ret$volnumber)] <- 1 

  # Set zero page counts to NA  
  ret$pagecount[is.na(ret$pagecount) | ret$pagecount == 0] <- NA 

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

  # if (verbose) { message(s) }
  if (is.na(s) || s == "s") { return(rep(NA, 3)) } 

  # Shortcut for easy cases: "24p."
  if (length(grep("^[0-9]+ {0,1}p\\.{0,1}$",s))>0) {
    return(c(as.numeric(str_trim(gsub(" {0,1}p\\.{0,1}$", "", s))), NA, NA))
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

  x[x == ""] <- NA
  x[x == "NA"] <- NA  

  # Return
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




