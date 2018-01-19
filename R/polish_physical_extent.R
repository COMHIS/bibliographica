#' @title Polish physical_extent Field
#' @description Pick page counts, volume counts and volume numbers.
#' @param x Page number field. Vector or factor of strings.
#' @param verbose Print progress info
#' @param mc.cores Number of cores for parallelization
#' @return Raw and estimated pages per document part
#' @details Document parts are separated by semicolons
#' @export
#' @details A summary of page counting rules that this function aims to (approximately)
#' implement are provided in 
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
  s <- gsub("^na ", "", s)
  s <- gsub("\\.s$", " s", s)
  s <- gsub("\\. s", " s", s)    
  s <- gsub("&", ",", s)
  s <- gsub("\\*", " ", s)
  s[grep("^[ |;|:|!|?]*$", s)] <- NA 

  # Remove dimension info
  s <- gsub("^[0-9]+.o ", "", s) 

  # In Finnish texts s. is used instead of p.		
  f <- system.file("extdata/translation_fi_en_pages.csv", package = "bibliographica")
  page.synonyms <- read_mapping(f, sep = ";", mode = "table", fast = TRUE)
  s <- map(s, page.synonyms, mode="match")
  rm(page.synonyms)

  f <- system.file("extdata/numbers_finnish.csv", package = "bibliographica")
  char2num <- read_mapping(f, sep = ",", mode = "table", from = "character", to = "numeric")
  s <- map(s, synonymes = char2num, from = "character", to = "numeric", mode = "match")
  rm(char2num)

  if (verbose) {message("Harmonize volume info")}
  #inds <- setdiff(1:length(s), setdiff(grep("v\\.$", s), grep("^v\\.$", s)))
  inds <- setdiff(1:length(s), grep("^v\\.$", s))
  if (length(inds)>0) {
    s[inds] <- remove_trailing_periods(s[inds])
  }

  # Harmonize volume info
  s <- unname(harmonize_volume(s))

  # Back to original indices and new unique reduction 
  sorig <- s[match(sorig, suniq)]
  s <- suniq <- unique(sorig)

  if (verbose) {message("Harmonize ie")}
  s <- harmonize_ie(s)

  s[s == ""] <- NA

  if (verbose) {message("Read the mapping table for sheets")}  
  f <- system.file("extdata/harmonize_sheets.csv", package = "bibliographica")
  sheet.harmonize <- read_mapping(f, sep = ";", mode = "table", fast = TRUE)

  s <- harmonize_sheets(s, sheet.harmonize)
  rm(sheet.harmonize)

  # Just read page harmonization here to be used later
  if (verbose) {message("Read the mapping table for pages")}
  f <- system.file("extdata/harmonize_pages.csv", package = "bibliographica")
  page.harmonize <- read_mapping(f, sep = "\t", mode = "table", fast = FALSE)

  # Back to original indices and new unique reduction 
  s <- s[match(sorig, suniq)]
  sorig <- s
  suniq <- unique(sorig)
  s <- suniq

  if (verbose) {message("Read the mapping table for romans")}  
  f <- system.file("extdata/harmonize_romans.csv", package = "bibliographica")
  romans.harm <- read_mapping(f, sep = "\t", mode = "table", fast = TRUE)
  s <- map(s, romans.harm, mode = "recursive")

  if (verbose) {message("Page harmonization part 2")}  
  f <- system.file("extdata/harmonize_pages2.csv", package = "bibliographica")
  harm2 <- read_mapping(f, sep = "|", mode = "table", fast = TRUE)
  s <- map(s, harm2, mode = "recursive")
  rm(harm2)

  # Trimming
  # p3 -> p 3
  inds <- grep("p[0-9]+", s)
  if (length(inds)>0) {
    s[inds] <- gsub("p", "p ", s[inds])
  }  
  s <- condense_spaces(s)
  s[s == "s"] <- NA

  # 1 score (144 p.) -> 144 pages 
  if (length(grep("[0-9]* *scores* \\([0-9]+ p\\.*\\)", s))>0) {
    s <- gsub("[0-9]* *scores*", " ", s)
  }
  s <- condense_spaces(s)
  
  if (verbose) {message("Polish unique pages separately for each volume")}  

  # Back to original indices and new unique reduction 
  sorig <- s[match(sorig, suniq)]
  s <- suniq <- unique(sorig)

  # English
  f <- system.file("extdata/numbers_english.csv", package = "bibliographica")
  char2num <- read_mapping(f, sep = ",", mode = "table", from = "character", to = "numeric")
  s <- map(s, synonymes = char2num, from = "character", to = "numeric", mode = "match")

  if (verbose) {message(paste("Polishing physical extent field 3:", length(suniq), "unique cases"))}
  ret <- lapply(s, function (s) { a <- try(polish_physext_help(s, page.harmonize)); if (class(a) == "try-error") {message(paste("Error in polish_physext_help:", s)); return(NA)} else {return(a)}})

  nainds <- which(is.na(ret))
  for (i in nainds) {
    # NA vector of same length than the other entries
    ret[[i]] <- rep(NA, length(ret[[1]])) 
  }

  if (verbose) {message("Make data frame")}
  ret <- as.data.frame(t(sapply(ret, identity)))

  if (verbose) {message("Set zero page counts to NA")}
  ret$pagecount <- as.numeric(ret$pagecount)  
  ret$pagecount[ret$pagecount == 0] <- NA

  if (verbose) { message("Project to original list") }
  ret[match(sorig, suniq), ]

}


#' @title Polish physical_extent Help Field
#' @description Internal
#' @param s input char
#' @return vector
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @keywords internal
polish_physext_help <- function (s, page.harmonize) {

  # Return NA if conversion fails
  if (length(s) == 1 && is.na(s)) {
    #return(rep(NA, 11))
    s <- ""
  } 

  #141-174. [2] -> "141-174, [2]"
  if (grepl("[0-9]+\\.", s)) {
    s <- gsub("\\.", ",", s)
  }

  # Shortcut for easy cases: "24p."
  if (length(grep("^[0-9]+ *p\\.*$",s))>0) {
    #return(c(as.numeric(str_trim(gsub(" {0,1}p\\.{0,1}$", "", s))), rep(NA, 9)))
    s <- as.numeric(str_trim(gsub(" {0,1}p\\.{0,1}$", "", s)))
  }

  # Pick volume number
  voln <- pick_volume(s) 

  # Volume count
  vols <- unname(pick_multivolume(s))

  # Parts count
  parts <- pick_parts(s)

  # "2 pts (96, 110 s.)" = 96 + 110s
  if (length(grep("[0-9]+ pts (*)", s)) > 0 && length(grep(";", s)) == 0) {
    s <- gsub(",", ";", s)
  }

  # Now remove volume info
  s <- suppressWarnings(remove_volume_info(s))

  # Cleanup
  s <- gsub("^;*\\(", "", s)
  s <- gsub(" s\\.*$", "", s)
  s <- condense_spaces(s)

  # If number of volumes is the same than number of comma-separated units
  # and there are no semicolons, then consider the comma-separated units as
  # individual volumes and mark this by replacing commas by semicolons
  # ie. 2v(130, 115) -> 130;115
  if (!is.na(s) && !is.na(vols) && length(unlist(strsplit(s, ","), use.names = FALSE)) == vols && !grepl(";", s)) {
    s <- gsub(",", ";", s)  
  }

  # Estimate pages for each document separately via a for loop
  # Vectorization would be faster but we prefer simplicity and modularity here

  if (length(grep(";", s)) > 0) {
    spl <- unlist(strsplit(s, ";"), use.names = FALSE)
    page.info <- sapply(spl, function (x) {polish_physext_help2(x, page.harmonize)})
    page.info <- apply(page.info, 1, function (x) {sum(as.numeric(x), na.rm = TRUE)})
    page.info[[1]] <- 1 # Not used anymore after summing up  
  } else {
    page.info <- polish_physext_help2(s, page.harmonize)
  }
  
  s <- page.info[["pagecount"]]
  page.info <- page.info[-7]
  s[s == ""] <- NA
  s[s == "NA"] <- NA  
  s <- as.numeric(s)
  s[is.infinite(s)] = NA

  # Return
  names(page.info) <- paste0("pagecount.", names(page.info))
  # Add fields to page.info  		   
  page.info[["pagecount"]] <- as.vector(s)
  page.info[["volnumber"]] <- as.vector(voln)
  page.info[["volcount"]] <- as.vector(vols)
  page.info[["parts"]] <- as.vector(parts)
  page.info <- unlist(page.info)
  
  page.info

}



#' @title Polish physical_extent help field 2
#' @description Internal
#' @param x Input char
#' @return Internal
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # TBA
#' @keywords internal
polish_physext_help2 <- function (x, page.harmonize) {

  # TODO: can we speed up by moving these up ?		     
  x <- as.character(map(x, page.harmonize, mode = "recursive"))

  if (length(grep("i\\.e", x)) > 0) {

    x <- unlist(strsplit(x, ","), use.names = FALSE)

    x <- sapply(x, function (x) {handle_ie(x, harmonize = FALSE)})

    x <- paste(x, collapse = ",")
    
  }

  # Remove endings
  x <- gsub("[ |\\.|\\,|\\;|\\:]+$", "", x)

  # Remove spaces around dashes
  x <- gsub(" {0,1}- {0,1}", "-", x)
  x <- condense_spaces(x)  

  # "[4] p. (p. [3] blank)" -> 4 p.
  if (length(grep("\\[[0-9]+\\] p \\(p \\[[0-9]+\\] blank\\)", x)) > 0) {
    x <- gsub(" \\(p \\[[0-9]+\\] blank\\)", "", x)            
  } else if (length(grep("^1 score \\([0-9]+ p\\)", x))>0) {
    # "1 score (144 p.)" -> 144p
    x <- gsub("1 score", "", x)
  }

  # Remove parentheses
  x <- gsub("\\(", " ", x)
  x <- gsub("\\)", " ", x)
  x <- condense_spaces(x)   
  x <- condense_spaces(gsub(" p p ", " p ", x))

  # 2 p [1] = 2, [1]
  if (length(grep("^[0-9]+ p \\[[0-9]+\\]$", x))>0) {
    x <- condense_spaces(gsub("\\[", ",[", gsub("p", "", x)))
  }

  # [4] p [4] = [4], [4]
  if (length(grep("^\\[[0-9]+\\] p \\[[0-9]+\\]$", x))>0) {
    x <- unlist(strsplit(x, "p"))[[1]]
  }

  # "[2] 4" -> "[2], 4"
  if (length(grep("\\[[0-9]+\\] [0-9]+", x))>0) {
    x <- gsub(" ", ",", x)
  }

  # "4 [2]" -> 4, [2]
  if (length(grep("^[0-9]+ \\[[0-9]+\\]", x))>0) {
    x <- gsub("\\[", ",[", x)    
  }

  if (length(grep("[0-9]+p",x))>0) {
    x <- condense_spaces(gsub("p", " p", x))
  }

  x <- gsub("p\\.*$", "", x)
  # [4] p 2:o -> 4
  x <- gsub("[0-9]:o$", "", x)
  x <- gsub("=$", "", x)
  x <- gsub("^[c|n]\\.", "", x)
  x <- gsub("p \\[", "p, [", x)
  x <- gsub(": b", ",", x)  
  x <- condense_spaces(x)
  x <- gsub(" ,", ",", x)
  x <- gsub("^,", "", x)    

  if (length(grep("\\[[[0-9]+\\] sheets*", x))>0) {
    n <- unlist(strsplit(x, "\\] sheets*"), use.names = FALSE)
    spl <- unlist(strsplit(n[[length(n)]], "\\["), use.names = F)
    n <- spl[[length(spl)]]    
    x <- gsub(paste("\\[", n, "\\] sheet", sep = ""), paste(", \\[", n, "\\] sheet", sep = ""), x)
  }

  x <- condense_spaces(x)

  page.info <- suppressWarnings(estimate_pages(x))

  # Take into account multiplier
  # (for instance when page string starts with Ff the document is folios
  # and page count will be multiplied by two - in most cases multiplier is 1)
  # Total page count; assuming the multiplier is index 1
  s <- unlist(page.info[-1], use.names = FALSE)
  page.info[["pagecount"]] <- page.info[["multiplier"]] * sum(s, na.rm = TRUE)

  page.info
  
}
