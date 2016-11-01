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
  s <- gsub("^na ", "", s)
  s <- gsub("\\.s$", " s", s)
  s <- gsub("\\. s", " s", s)    
  s <- gsub("&", ",", s)  
  s[grep("^[ |;|:|!|?]*$", s)] <- NA 

  # Remove dimension info
  s <- gsub("^[0-9]+.o ", "", s) 

  # In Finnish texts s. is used instead of p.		
  f <- system.file("extdata/translation_fi_en_pages.csv", package = "bibliographica")
  page.synonyms <- read_mapping(f, sep = ";", mode = "table", fast = TRUE)
  s <- map(s, page.synonyms, mode="match")
  rm(page.synonyms)

  if (verbose) {message("Harmonize volume info")}
  inds <- setdiff(1:length(s), setdiff(grep("v\\.$", s), grep("^v\\.$", s)))
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

  if (verbose) {message("Recognize missing page cases")}
  # List cases that are considered as documents with missing pages
  # For instance: documents with only plates info, or gatherings
  # are here considered as books with missing page count info.
  # Plates
  plates <- read.csv(system.file("extdata/plates.csv",
  	              package = "bibliographica"), header = FALSE,
		      stringsAsFactors = FALSE)[,1]		      
  # Gatherings
  gats <- unlist(gatherings_table()[, c("Alternate", "Standard", "Symbol")], use.names = F)
  # Combine
  missing <- unique(c(plates, gats))
  # Remove
  s <- gsub(",", ", ", s)
  s2 <- condense_spaces(suppressWarnings(gsub("^[\\.|\\,]", "", remove_volume_info(s))))
  inds <- which(s2 %in% missing)
  for (m in missing) {
    s[inds] <- gsub(m, " ", s[inds])
  }
  s <- condense_spaces(s)
  rm(missing)  
  s[s == ""] <- NA


  if (verbose) {message("Read the mapping table for pages")}
  f <- system.file("extdata/harmonize_pages.csv", package = "bibliographica")
  page.harmonize <- read_mapping(f, sep = "\t", mode = "table", fast = FALSE)

  if (verbose) {message("Read the mapping table for sheets")}  
  f <- system.file("extdata/harmonize_sheets.csv", package = "bibliographica")
  sheet.harmonize <- read_mapping(f, sep = ";", mode = "table", fast = TRUE)
  s <- harmonize_sheets(s, sheet.harmonize)
  rm(sheet.harmonize)

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
  inds = grep("p[0-9]+", s)
  if (length(inds)>0) {
    s[inds] <- gsub("p", "p ", s[inds])
  }  
  s <- condense_spaces(s)
  s[s == "s"] <- NA

  if (verbose) {message("Polish unique pages separately for each volume")}  

  # Back to original indices and new unique reduction 
  sorig <- s[match(sorig, suniq)]
  s <- suniq <- unique(sorig)

  if (verbose) {message(paste("Polishing physical extent field 3:", length(suniq), "unique cases"))}

  ret <- parallel::mclapply(s, function (s) { a <- try(polish_physext_help(s, page.harmonize)); if (class(a) == "try-error") {return(NA)} else {return(a)}}, mc.cores = mc.cores)

  if (verbose) {message("Make data frame")}
  ret <- as.data.frame(t(sapply(ret, identity)))
  names(ret) <- c("pagecount", "volnumber", "volcount", "parts")

  if (verbose) {message("Set zero page counts to NA")}    
  ret$pagecount[ret$pagecount == 0] <- NA 

  # When volcount not given but parts is given, interpret parts as volumes
  # and remove parts info (to avoid confusion)
  #inds = is.na(ret$volcount) & !is.na(ret$parts)
  #ret[inds, "volcount"] = ret[inds, "parts"]
  #ret[inds, "parts"] = NULL

  if (verbose) { message("Project to original list") }
  ret[match(sorig, suniq), ]

}


#' @title Polish physical_extent help field
#' @description Internal
#' @param s Input char
#' @return Internal
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @keywords internal
polish_physext_help <- function (s, page.harmonize) {

  # Return NA if conversion fails
  if (length(s) == 1 && is.na(s)) { return(rep(NA, 4)) } 

  # Shortcut for easy cases: "24p."
  if (length(grep("^[0-9]+ {0,1}p\\.{0,1}$",s))>0) {
    return(c(as.numeric(str_trim(gsub(" {0,1}p\\.{0,1}$", "", s))), NA, NA, NA))
  }

  # Pick volume number
  voln <- pick_volume(s) 

  # Volume count
  vols <- pick_multivolume(s)

  # Parts count
  parts <- pick_parts(s)

  # "2 pts (96, 110 s.)" = 96 + 110s
  if (length(grep("[0-9]+ pts (*)", s)) > 0 && length(grep(";", s)) == 0) {
    s = gsub(",", ";", s)
  }

  # "3 vol (16, 16, 16 s.)" becomes 16 + 16 + 16
  if (length(grep("[0-9]+ vol (*)", s)) > 0 && length(grep(";", s)) == 0) {
    s = gsub(",", ";", s)
  }

  # Now remove volume info
  s <- suppressWarnings(remove_volume_info(s))

  # Estimate pages for each document separately via a for loop
  # Vectorization would be faster but we prefer simplicity and modularity here

  # Pagecount per semicolon separated unit
  if (length(grep(";", s)) > 0) {
    spl <- unlist(strsplit(s, ";"), use.names = FALSE)
    s <- try(unname(sapply(spl, function (x) {polish_physext_help2(x, page.harmonize)})))
  } else {
    s <- polish_physext_help2(s, page.harmonize)
  }

  if (class(s) == "try-error") {
    s <- NA
  } 

  s[s == ""] <- NA
  s[s == "NA"] <- NA  
  s <- as.numeric(s)
  s[is.infinite(s)] = NA
  
  # Return
  c(sum(s, na.rm = TRUE), voln, vols, parts)  

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

  if (length(grep("[0-9]+p",x))>0) {
    x <- condense_spaces(gsub("p", " p", x))
  }

  x <- gsub("p\\.*$", "", x)
  x <- condense_spaces(x)

  x <- suppressWarnings(estimate_pages(x))

  x
  
}
