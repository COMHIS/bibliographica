#' @title polish_volumenumber
#' @description Get volume number from page field if available
#'
#' @param s Page number field. Vector or factor of strings.
#' @return Volume number
#' @details Refers to single-volume document where the volume has been specified
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples polish_volumenumber("v.4")
#' @keywords utilities
polish_volumenumber <- function (s) {

  # A summary of page counting rules that this function aims to (approximately) implement
  # https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html
  s <- as.character(s)

  # Harmonize volume info
  s <- harmonize_volume(s)
  
  #' A single document, but check which volume ?
  # (document starting with 'v.*')
  voln <- sapply(s, function (x) {pick_volume(x)})

  voln

}


#' @title polish_volumecount
#' @description Get volume number from page field if available
#'
#' @param s Page number field. Vector or factor of strings.
#' @return Number of volumes
#' @details Refers to multi-volume document where the number of volumes has been specified
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples polish_volumecount("4v.")
#' @keywords utilities
polish_volumecount <- function (s) {

  # A summary of page counting rules that this function aims to (approximately) implement
  # https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html
  s <- as.character(s)

  # Harmonize volume info
  s <- harmonize_volume(s)

  # Pick multi-volume information 
  # (document starting with '* v.' or 'v.1-3' etc.)
  vols <- sapply(s, function (x) {pick_multivolume(x)})

  # Assume single volume when number not given
  vols[is.na(vols)] <- 1 

  vols

}



#' @title polish_pages
#' @description clean up page numbers
#'
#' @param s Page number field. Vector or factor of strings.
#' @param verbose Print progress info
#' @return Raw and estimated pages per document part
#' @details Document parts are separated by semicolons
#'
#' @export
#' 
#' @details A summary of page counting rules that this function aims to (approximately) implement
#'          are provided in \url{https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html}
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples # polish_pages("4p.")
#' @keywords utilities
polish_pages <- function (s, verbose = FALSE) {

 # A summary of page counting rules that this function aims to (approximately) implement
  # https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html

  # Summary of abbreviations
  # http://ac.bslw.com/community/wiki/index.php5/RDA_4.5

  s <- as.character(s)

  # Estimate pages
  raw <- sp <- list()
  for (i in 1:length(s)) {
    if (verbose) {message(i)}

    a <- try(pp <- polish_page(s[[i]]))

    if (is.character(a) && a == "try-error") {
      sp[[i]] <- NA
      raw[[i]] <- s[[i]]
    } else {
      ss <- unname(unlist(pp$pages))
      ss[is.infinite(ss)] <- NA
      sp[[i]] <- ss
      raw[[i]] <- unname(unlist(pp$raw))
    }
  }

  # Collapse page counts
  # sp2 <- sapply(sp, function (s) { paste(s, collapse = ";") })
  list(estimated.pages = sp, raw.pages = raw)

}




harmonize_volume <- function (s) {
  s[s == "^v\\. ;"] <- NA
  s <- gsub("^Vol\\.", "v.", s)
  s <- gsub("^vols\\.", "v.", s)
  s <- gsub("^Vols\\.", "v.", s)
  s <- gsub("^Pp\\.", "p.", s)
  s <- gsub("^v\\. ", "v.", s)
  s <- gsub("^v\\.\\(", "(", s)
  s <- gsub("^v\\.,", "", s)
  s <- gsub("v\\.:bill\\. ;", NA, s)
  s
}


#' @title pick_volume
#' @description Pick volume
#'
#' @param s Page number field. Vector or factor of strings.
#' @return Volume
#'
#' @export
#' 
#' @details A single document, but check which volume 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples pick_volume("v.4")
#' @keywords utilities
pick_volume <- function (s) {

  # Remove some rare misleading special cases manually
  s <- gsub("v.1-3, 5 ;", "", s)
  s <- gsub("v.1,4-7 ;", "", s)

  vol <- NA	    
  if (length(grep("^v\\.", s)) > 0) {
    s <- gsub("^v\\.", "", s)
    i <- 1
    n <- as.numeric(substr(s, 1, 1))
    while (i <= nchar(s) && !is.na(n)) {
      n <- as.numeric(substr(s, 1, i))
      # Pick cases v.1 but not v.1-3
      if (!is.na(n) && !substr(s, i+1, i+1) == "-") {
        vol <- n
      } else if (substr(s, i+1, i+1) == "-") {
        vol <- NA
      } else {
        i <- Inf
      }

      i <- i+1
    }
  }

  vol
}

# Number of volumes
pick_multivolume <- function (x) {

  s <- as.character(x)

  # v.1-3 -> 3
  vol <- check_volumes(s)$n

  # v.1 -> 1
  if (is.null(vol)) {
    vol <- NA	   
    inds <- grep("v\\.", s)
    if (length(inds) > 0) {
      # FIXME: SPLITMEHERE used as a quick fix as v\\. was unrecognized char and
      # causes error
      s2 <- gsub("v\\.", "SPLITMEHERE", s)
      #vol <- as.numeric(str_trim(unlist(strsplit(s, "v\\."))[[1]]))
      vol <- as.numeric(str_trim(unlist(strsplit(s2, "SPLITMEHERE"))[[1]]))
    }
  }

  vol

}

#' @title polish_page
#' @description clean up page numbers
#'
#' @param x Page number field. Vector or factor of strings.
#' @return Cleaned up version of page number field.
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples # polish_page("4p.")
#' @keywords internal
polish_page <- function (x) {
	    
  s <- as.character(x)

  # Pick number of volumes
  vols <- polish_volumecount(s)

  # Remove volume info
  s <- remove_volume_info(s)

  # Split by semicolon (each element would be one volume)
  spl <- str_trim(unlist(strsplit(s, ";")))
  if (length(spl) == 0) { spl <- NA } 

  # Asses pages for each volume
  pages <- sapply(spl, function (x) { estimate_pages(x) })

  list(raw = spl, pages = pages)
 
}


remove_volume_info <- function (s) {

  # Remove some rare special cases manually
  s <- gsub("v.1-3, 5 ;", "", s)
  s <- gsub("v.1,4-7 ;", "", s)
  s <- gsub("v.6-7,9-12", "", s)
  s <- gsub("Vols.6-7,9-12,plates :", "plates", s)
  s <- gsub("^v\\.:", "", s)
  s <- gsub("^v\\.\\,", "", s)
  s <- gsub("^v\\,", "", s)

  # Pick and remove multi-volume information (document starting with '* v.')
  vols <- pick_multivolume(s)  
  # Then remove the volume information that was picked
  s <- gsub(paste("^", vols, " v.", sep = ""), paste(vols, "v.", sep = ""), str_trim(s))
  s <- str_trim(gsub(paste("^", vols, "v.", sep = ""), "", s)) 
  s <- str_trim(gsub("^,", "", s))

  # Cases 'v.1-3' etc
  inds <- intersect(grep("^v.", s), grep("-", s))
  for (i in inds) {
    s[[i]] <- gsub(check_volumes(s[[i]])$text, "", s[[i]])
  }

  # Pick which volume this might be (if given)
  # Cases 'v.1' etc.
  voln <- pick_volume(s)
  # Then remove the volume information that was picked
  s <- str_trim(gsub(paste("v.", voln, ":", sep = ""), "", s))
  s <- str_trim(gsub(paste("v.", voln, sep = ""), "", s))

  # "v. (183,[2]) -> (183,[2])"
  s <- gsub("^v. ", "v.", s)
  s <- gsub("^v.\\(", "(", s)

  s

}


# v.1-3 -> 3
check_volumes <- function (x) {

  nvol <- vtext <- NULL
  n2 <- n1 <- NULL

  # Handle some rare special cases manually
  if (is.na(x)) {
    nvol <- NA
    vtext <- NA
  } else if (x == "v.1-3, 5 ;") {
    nvol <- 4
    vtext <- "v.1-3,5"
  } else if (x == "v.1,4-7 ;") {
    nvol <- 5
    vtext <- "v.1,4-7"
  } else if (x == "Vols.6-7,9-12,plates :") {
    nvol <- 6
    vtext <- "v.6-7,9-12"
  } else if (length(grep("^v.", x)) > 0 && length(grep("-", x)) > 0) {
    x <- gsub("^v.", "", x)
    x2 <- unlist(strsplit(x, "-"))
    n1 <- as.numeric(x2[[1]])

    i <- 1
    n <- as.numeric(substr(x2[[2]], 1, i))
    while (is.numeric(n) && i <= nchar(x2[[2]])) {
      n2 <- n
      n <- as.numeric(substr(x2[[2]], 1, i))
      i <- i+1
    }

    # Number of volumes
    nvol <- n2 - n1 + 1
 
    # Volume statement
    vtext <- paste("v.", n1, "-", n2, sep = "")

  }

  list(n = nvol, text = vtext)
 
}



harmonize_pages <- function (s) {

  s <- gsub("Caption title; with a docket title that reads 'Memorial of the agent for the province of Massachussetts-Bay against a duty of 3d. per gallon on foreign molasses.'. - Dated in MS '9th February, 1764' and signed J. Mauduit.", NA, s)
  s <- gsub("5 v. ; 42-43 cm \\(2⁰\\)", "5 v.;", s)
  s <- gsub("\\#\\,", ",", s)
  s <- gsub("\\(v.1: \\[72\\], 658, \\[32\\] p.; v.2: \\[144\\], 530, \\[14\\]; 237, \\[1\\] p.; v.3: \\[116\\], 465, \\[21\\]; 370, \\[2\\] p.\\) ;", "(v.1: [72], 658, [32] p.; v.2: [144], 530, [14], 237, [1] p.; v.3: [116], 465, [21], 370, [2] p.) ;", s)
  s <- gsub("2 v. \\(v.1: \\[8\\], 124 \\[i.e. 126\\], \\[1\\] leaves; 289, \\[1\\], \\[8\\], 22, \\[2\\], 518, \\[26\\] p., \\[2\\], 28 leaves, 115, \\[9\\] p.; v.2: \\[4\\], 291-659, 700-981, 990-1593, \\[1\\], 1593-1876, \\[104\\] p., folded plate\\) :", "2 v. (v.1: [8], 124 [i.e. 126], [1] leaves, 289, [1], [8], 22, [2], 518, [26] p., [2], 28 leaves, 115, [9] p.; v.2: [4], 291-659, 700-981, 990-1593, [1], 1593-1876, [104] p., folded plate) :", s)
  s <- gsub("2 v. \\(v.1: \\[8\\], 124 \\[i.e. 126\\], \\[1\\] leaves; 289, \\[1\\]; \\[8\\], 22, \\[2\\]; 518, \\[26\\] p.; \\[2\\], 28 leaves; 115, \\[9\\] p.; v.2: \\[4\\], 291-659, 700-981, 990-1593, \\[1\\], 1593-1876; \\[104\\] p., folded plate\\) :", "2 v. (v.1: [8], 124 [i.e. 126], [1] leaves, 289, [1], [8], 22, [2], 518, [26] p., [2], 28 leaves, 115, [9] p.; v.2: [4], 291-659, 700-981, 990-1593, [1], 1593-1876, [104] p., folded plate) :", s)
  s <- gsub("3 v. \\(vol. 1: \\[10\\], 250; \\[4\\], 202, \\[2\\] p.; vol. 2: 61, \\[13\\], 183, \\[1\\]; 421, 424-430, 436-438, 431-433, 439-445, 450-464, \\[56\\] p.; vol. 3: \\[8\\], 1080, 1080-1327, \\[2\\], 1332-1371, 1371-1421, 1490-1491, 1536-1555, 1574-1592, \\[62\\] p.\\) ;", "3 v. (vol. 1: [10], 250, [4], 202, [2] p.; vol. 2: 61, [13], 183, [1], 421, 424-430, 436-438, 431-433, 439-445, 450-464, [56] p.; vol. 3: [8], 1080, 1080-1327, [2], 1332-1371, 1371-1421, 1490-1491, 1536-1555, 1574-1592, [62] p.) ;", s)
  s <- gsub("p\\.plates", "p., plates", s) # "39,[1]p.plates :" -> "39,[1]p.,plates :"
  s <- gsub("\\)plates :", "),plates", s)
  s <- gsub("p\\.table", "p., table", s)
  s <- gsub("p\\. ", "p., ", s)
  s <- gsub("3\\.", "3,", s)
  s <- gsub("p\\., of plates", "plates", s)
  s <- gsub("c1 \\.", "", s)

  # Add missing commas
  s <- gsub("\\[8\\] 140 p\\.", "[8], 140", s)
  s <- gsub("\\[8\\] 182", "[8], 182", s)
  s <- gsub("\\[16\\] 240", "[16], 240", s)

  # TODO formulate rule for this
  s <- gsub("233-248 leaves", "233-248,", s)
  s <- gsub("205-216 leaves", "205-216,", s)
  s <- gsub("107-133 leaves", "107-133,", s)

  # Rare cases
  s <- gsub("32t p\\.", "32 p.", s)
  s <- gsub("\\[1⁺\\]", "[1]", s)
  s <- gsub("1\\/ \\.$", ",1", s)
  s <- gsub("1/", " ", s)
  s <- gsub("c1⁰\\.$", "", s)
  s <- gsub("\\[x\\]", " ", s)
  s <- gsub("\\+\\]", "]", s)
  s <- gsub("\\[2\\] single and \\[8\\] double leaves of plates", "[2],[8]", s)
  s <- gsub("\\[fewer than 50 pages\\]", NA, s)
  s <- gsub("obl\\.1/[0-9]⁰\\.", NA, s)
  s <- gsub("long [0-9][0-9]⁰\\.", NA, s)
  s <- gsub("[0-9][0-9]-[0-9][0-9] cm\\. \\([0-9]⁰; [0-9]⁰\\)", " ", s)
  s <- gsub("[0-9][0-9]-[0-9][0-9] cm\\. \\([0-9]⁰", " ", s)
  s <- gsub("[0-9]/[0-9]⁰-[0-9]⁰\\.", " ", s)
  s <- gsub("[0-9]⁰ \\([0-9][0-9]\\,[0-9] cm\\.\\)", " ", s)
  s <- gsub("[0-9]⁰ \\([0-9][0-9]\\.[0-9] cm\\.\\)", " ", s)
  s <- gsub("[0-9][0-9] cm\\.\\([0-9][0-9]⁰\\)", " ", s)
  s <- gsub("\\([0-9][0-9] cm\\.\\) [0-9][0-9]⁰", " ", s)
  s <- gsub("[0-9][0-9] cm\\.\\([0-9]⁰\\)", " ", s)
  s <- gsub("[0-9][0-9] cm\\. \\([0-9]\\)", " ", s)
  s <- gsub("[0-9][0-9] cm\\. \\(1/[0-9]⁰\\)", " ", s)
  s <- gsub("[0-9][0-9] cm\\. \\([0-9]to\\)", " ", s)
  s <- gsub("[0-9]⁰ \\([0-9][0-9] cm\\.\\)", " ", s)
  s <- gsub("[0-9]⁰ \\([0-9][0-9]-[0-9][0-9] cm\\.\\)", " ", s)
  s <- gsub("[0-9]⁰\\)", " ", s)
  s <- gsub("[0-9][0-9]⁰\\.f", " ", s)
  s <- gsub("[0-9]⁰ in [0-9]\\.", " ", s)
  s <- gsub("[0-9][0-9]⁰", " ", s)
  s <- gsub("[0-9][0-9] ⁰", " ", s)
  s <- gsub("[0-9] ⁰", " ", s)
  s <- gsub("[0-9]⁰", " ", s)
  s <- gsub("[0-9] D[0-9]\\.", " ", s)
  s <- gsub("[0-9][0-9] cm\\.", " ", s)
  s <- gsub("[0-9]to", " ", s)
  s <- gsub("[0-9]vo", " ", s)
  s <- gsub(" and ", " ", s)

  # Romans
  s <- gsub("leaf lxxxvij", "lxxxvij", s)
  s <- gsub("leaf C.xxxv", "C.xxxv", s)
  s <- gsub("CVXI", "CXVI", s) # Typo ?
  s <- gsub("c\\.lii", "clii", s) 
  s <- gsub("C\\.", "C", s) 
  s <- gsub("Cl\\.", "Cl", s) 
  s <- gsub("\\.\\]", "]", s) 
  s <- gsub("xxvii\\.", "xxvii", s) 

  # l.
  s <- gsub(" 1 l\\.", "leaf ", s) # 
  s <- gsub("\\[1\\] l\\.", "leaf ", s) # 
  s <- gsub("\\,l\\.", ",leaves ", s) # 
  s <- gsub(" l\\.", "leaves ", s) # 

  # Remove endings
  for (i in 1:5) {
    s <- remove_endings(s, c(" ", "\\.", "\\,", "\\;", "\\:"))
  }

  # Harmonize page info
  s <- gsub("Pp\\.", "p", s)
  s <- gsub("pp\\.", "p", s)
  s <- gsub("p\\.", "p", s)
  s <- gsub("p ", "p", s)
  s <- gsub("p$", "p", s)
  s <- gsub("P\\.", "p", s)
  s <- gsub("P ", "p", s)
  s <- gsub("P$", "p", s)
  s <- gsub("^p", "", s)

  s <- gsub("\\(versos blank\\)", " ", s)
  s <- gsub("\\(woodcut\\)", " ", s)
  s <- gsub("platess", "plates", s)
  s <- gsub("leaf of plates folded", "leaf", s)
  s <- gsub("leaves of plates folded", "leaf", s)
  s <- gsub("leaves of plates \\(maps\\)", "leaf", s)
  s <- gsub("leaves folded", "leaf", s)
  s <- gsub("fold\\. leaf of plates", "leaf", s)
  s <- gsub("folded leaf plates", "leaf", s)
  s <- gsub("folding leaf of plate", "leaf", s)
  s <- gsub("leaf plates", "leaf", s)
  s <- gsub("folded leaf", "leaf", s)
  s <- gsub("folded leaves", "leaves", s)
  s <- gsub("folded plates", "plates", s)
  s <- gsub("\\([0-9][0-9] folded\\)", " ", s)
  s <- gsub("\\([0-9] folded\\)", " ", s)
  s <- gsub("\\(some fold., col.\\)", " ", s)
  s <- gsub("\\(some fold\\., some col\\.\\)", " ", s)
  s <- gsub("\\(some col.\\)", " ", s)
  s <- gsub("\\(front\\.\\)", " ", s)
  s <- gsub("\\(some fold.\\)", " ", s)
  s <- gsub("\\([0-9] fold.\\)", " ", s)
  s <- gsub("\\([0-9] fold\\)", " ", s)
  s <- gsub("\\(some folded\\)", " ", s)
  s <- gsub("\\(most folded\\)", " ", s)
  s <- gsub("\\(one folded\\)", " ", s)
  s <- gsub("\\(folded\\)", " ", s)
  s <- gsub("\\(folding\\)", " ", s)
  s <- gsub("\\(some fold\\)", " ", s)
  s <- gsub("\\(fol\\.\\)", " ", s)
  s <- gsub("\\(\\[[0-9]\\] folded\\)", " ", s)
  s <- gsub("\\([0-9] folded\\)", " ", s)
  s <- gsub("fold\\. plates", "plates", s)
  s <- gsub("folding plates", "plates", s)
  s <- gsub("folding", "plates", s)
  s <- gsub("folded plate", "plate", s)
  s <- gsub("leaves \\([0-9] folded\\)", "leaves ", s)

  # Harmonize
  s <- gsub("\\[No pagination provided\\]", " ", s)
  s <- gsub("folded sheet", "sheet", s)
  s <- gsub("1 sheet \\(\\[1\\] p\\)", "1 sheet", s)
  s <- gsub("1 sheet \\(\\[2\\] p\\)", "1 sheet", s)
  s <- gsub("1 sheet \\(\\[1\\] page\\)", "1 sheet", s)
  s <- gsub("1 sheet \\(\\[2\\] pages\\)", "1 sheet", s)
  s <- gsub("1 sheet \\(1 page\\)", "1 sheet", s)
  s <- gsub("1 sheet \\(2 pages\\)", "1 sheet", s)
  s <- gsub("1 sheet \\[\\(1\\) p\\.\\]", "1 sheet", s)
  s <- gsub("1sheet", "1 sheet", s)

  # Harmonize '* sheets'
  spl <- strsplit(s, ",")
  sheet.inds <- grep("sheet", spl)
  for (i in sheet.inds) {
    if (length(grep("^[0-9] sheets", s)) > 0) {
      spl[[i]] <- paste(as.numeric(str_trim(unlist(strsplit(spl[[i]], "sheet"))[[1]])), "sheets", sep = " ")
    }
    s <- paste(spl, collapse = ",")
    s <- gsub("1 sheets", "1 sheet", s)
  }

  s <- gsub("pate", "plate", s)
  s <- gsub("pleave", "leave", s)
  s <- gsub("plates plates", "plates", s)
  s <- gsub("leaf of plates", "leaf", s)
  s <- gsub("leaf of plate", "leaf", s)
  s <- gsub("leaves of plates", "leaves", s)
  s <- gsub("leaves of plate", "leaves", s)
  s <- gsub("leafs", "leaves", s)

  # Table
  s <- gsub("folded genealogical table", "table", s)
  s <- gsub("folded table", "table", s)
  s <- gsub("fold\\. table", "table", s)
  s <- gsub("fold\\.tables", "table", s)
  s <- gsub("table", "plate", s)
  s <- gsub("plate", "sheet", s)

  # Double
  s <- gsub("\\([0-9] double\\)", " ", s)
  s <- gsub("doubled", " ", s)
  s <- gsub("double", " ", s)
  s <- gsub("single", " ", s)

  # Harmonize broadside
  s <- gsub("broadside of ill.", "broadside", s)
  s <- gsub("broadside ([1] p.)", "broadside", s)
  s <- gsub("broadsheet", "broadside", s)

  # Quarto etc?
  # Set these to NA
  # and afterwards assign some estimated page count given for 
  # books of that size
  s <- gsub("quarto", " ", s) # 
  s <- gsub("broadside", " ", s) # 
  s <- gsub("folios", " ", s) # 
  s <- gsub("folio", " ", s) # 
  s <- gsub("\\(fol.\\)", " ", s) # 

  # TODO check later how to account for this
  # Remove "bis", "*", "+" for now
  # ignore "+" 
  s <- gsub("\\+", "", s)    
  s <- gsub("bis", "", s)
  s <- gsub("\\*", "", s)
  s <- gsub("\\] p. \\[", "] p., [", s)
  s <- gsub("\\[\\?\\]", " ", s)
  s <- gsub("\\?", " ", s)

  # Handle some rare strange cases
  s <- gsub("53a-62k", "53-62", s)
  s <- gsub("\\:bill", " ", s)
  s <- gsub("\\bill", " ", s)
  s <- gsub("\\?\\]", "\\]", s) # [8?]
  s <- gsub("^l", "", s) # 

  # maps count as normal pages
  s <- gsub("\\(map", "map", s)
  s <- gsub("map\\)", "map", s)
  s <- gsub("maps\\)", "map", s)
  s <- gsub("map8⁰", "map", s)
  s <- gsub("folded map", "map", s)
  s <- gsub("map", " 1p", s)

  # Remove page notation
  s <- gsub("p$", "", s)
  s <- gsub("p\\,", "\\,", s)
  s <- gsub("^p[0-9][0-9][0-9]", "", s)
  s <- gsub("^p[0-9][0-9]", "", s)
  s <- gsub("^p[0-9]", "", s)
  s <- gsub("^p ", "", s)

  # Remove parentheses
  s <- str_trim(gsub("\\)", " ", gsub("\\(", " ", s)))

  # Harmonize i.e.
  s <- gsub("i\\. e", "i.e", s)
  s <- gsub("\\[i\\.e", " i.e", s)
  s <- gsub("\\[ie\\.", " i.e", s)
  s <- gsub("\\,i\\.e", "i.e", s)
  s <- gsub("\\, i\\.e", "i.e", s)
  s <- gsub("i\\.e", "i.e.", s)
  s <- gsub("i\\.e\\.\\.", "i.e.", s)
  s <- gsub("i\\.e\\.\\,", "i.e.", s)
  s <- gsub("i\\.e\\.", "i.e ", s)

  # Add comma with brackets
  s <- gsub("\\[", ",[", s)
  s <- gsub("\\(", ",(", s)

  # blank
  s <- gsub("\\[1 blank\\]", "[1]", s)

  # NOTE: in some systems lvii -> lvij etc. Handle this:
  s <- gsub("ij", "ii", s) 
  s <- gsub("xj", "xi", s) 
  s <- gsub("C\\.", "C", s) 
  s <- str_trim(gsub("^,", "", s))

  if (s == "") { s <- NA }

  s

}

# A single instance of pages within commas
harmonize_pages_by_comma <- function (s) {

  # Harmonize '1 sheet'
  if (length(grep("1 sheet", s)) > 0 || s == "sheet") {
    s <- "1 sheet" 
  }

  # Harmonize '* sheets'
  if (length(grep("^[2-9] sheets", s)) > 0) {
    s <- paste(as.numeric(str_trim(unlist(strsplit(s, "sheet"))[[1]])), "sheets", sep = " ")
  }

  # Harmonize '1 broadside'
  if (length(grep("1 broadside", s)) > 0) {
    s <- "1 broadside" 
  }

  # 165-167 leaves -> 165-167
  if (length(grep("-", s))>0 && length(grep("leaves", s))>0) {
    s <- str_trim(gsub("leaves", "", s))
  }

  # Convert plates to pages
  s <- plates2pages(s)

  # After plate operations handle p
  if (length(grep("plates", s)) == 0) {
    s <- gsub("pages", " ", s)
    s <- gsub("page", " ", s)
    s <- gsub("p\\.\\)", " ", s)
    s <- gsub("p\\.", " ", s)
    s <- gsub("p", " ", s)
  }

  # Handle some odd cases
  s <- gsub("a-m", " ", s)
  s <- polish_ie(s)
  s <- str_trim(s)

  s

}


polish_ie <- function (x) {
  # Handle ie
  if (length(grep("i\\.e", x)) > 0) {
    x <- gsub("\\]", " ", str_trim(unlist(strsplit(x, "i.e")))[[2]])
  }
  x
}

is.roman <- function (x) {

  x <- gsub("\\.", NA, x)

  check.roman <- function (x) {

    xs <- unlist(strsplit(x, "-"))
    isr <- c()
    for (i in 1:length(xs)) {  
      x <- xs[[i]]
      tmp <- suppressWarnings(as.numeric(x))
      tmp2 <- suppressWarnings(as.numeric(as.roman(x)))
      not.numeric <- length(na.omit(tmp)) > 0
      roman.numeric <- is.numeric(tmp2)

      isr[[i]] <- !(not.numeric && roman.numeric) && !is.na(tmp2) 
    }
    # iii-7 TRUE; iii-iv TRUE; 4-7 FALSE
    any(isr)
  }

  sapply(x, check.roman)

}




pages2arabics <- function (s) {

  # Convert romans to arabics (entries separated by spaces possibly)
  spl <- str_trim(unlist(strsplit(s, " ")))

  ns <- c()
  romans <- rep(FALSE, length(spl))
  for (i in 1:length(spl)) {

    x <- spl[[i]]

    if (length(grep("-", x)) > 0) {
      x2 <- str_trim(unlist(strsplit(x, "-")))
      n <- as.numeric(as.roman(x2))
      n[is.na(n)] <- x2[is.na(n)] # vii-160
      if (any(is.roman(x2))) {
        romans[[i]] <- TRUE
      }
      x <- paste(n, collapse = "-")
    } else {
      if (is.roman(x)) {
        romans[[i]] <- TRUE
      }
      x <- as.numeric(as.roman(x))
    }
  
    ns[[i]] <- x

  }

  nums <- !is.na(ns)

  if (sum(nums) > 0) {
    spl[nums] <- ns[nums]
    romans[nums] <- romans[nums]
    arabic <- spl[!romans]
    roman <- spl[romans]
  } else {
    arabic <- roman <- roman.logical <- NA
  }

  res <- list(converted.numbers = spl, roman = roman, arabic = arabic, roman.logical = romans)

  res
}


all2arabics <- function (x) {

  # Convert all non-plate pages into arabics (arabics, romans, square
  # brackets, dashed) in the order of occurrence
  # Remove dashes
  xseries <- str_trim(unlist(strsplit(x, "-")))

  # Remove square brackets
  xseries <- str_trim(gsub("\\]", " ", gsub("\\[", " ", xseries)))  

  # Convert to arabics
  xseries <- sapply(xseries, function (x) as.numeric(as.roman(x)))

  xseries
  
}


is.increasing <- function (x) {
  # Test if the numeric series is monotonically increasing
  incr <- FALSE
  if (!all(is.na(x))) {
    incr <- all(diff(x) >= 0)
  }

  incr
}

remove.squarebrackets <- function (x) {
  str_trim(gsub("\\[", "", gsub("\\]", "", x)))
}

estimate_pages <- function (x) {

  # Harmonize synonymes and spelling errors	       
  x <- harmonize_pages(x)

  if (all(is.na(x))) {
    # NA
    return(NA)
  } else if (!is.na(suppressWarnings(as.numeric(x)))) {
    # 3
    return(as.numeric(x))
  } else if (is.roman(x)) {
    # III
    return(as.numeric(as.roman(x)))
  } else if (!is.na(suppressWarnings(as.numeric(remove.squarebrackets(x))))) {
    # [3]
    return(as.numeric(remove.squarebrackets(x)))
  } else if (x == "1 sheet") {
    return(2)
  }

  # In some cases the semicolon separated pages have also volumes
  # listed: "3 v. (v.1: [72], 658, [32] p.; v.2: [144], 530, [14];
  #  237, [1] p.; v.3: [116], 465, [21]; 370, [2] p.) ;"
  # So remove here
  x <- remove_volume_info(x)

  # Handle contents as comma-separated
  spl <- str_trim(unlist(strsplit(x, ",")))
  spl <- gsub(" -", "-", spl)
  spl <- gsub("- ", "-", spl)

  # Harmonize within each comma
  x <- sapply(spl, function (x) { harmonize_pages_by_comma(x) })
  x[x == ""] <- NA
  x <- as.vector(na.omit(x))
  # Now x is a vector of page strings

  # Pick separately pages estimated from plates
  inds.plates <- grep("pages calculated from plates", x)
  x.plates <- 0
  if (length(inds.plates) > 0) { 
    x.plates <- x[inds.plates]
    x <- x[setdiff(1:length(x), inds.plates)]
  }
  x.plates <- sum(na.omit(suppressWarnings((as.numeric(str_trim(gsub("pages calculated from plates", "", x.plates)))))))

  # Pick separately pages estimated from sheets
  inds.sheets <- grep("sheet", x)
  x.sheets <- 0
  if (length(inds.sheets) > 0) { 
    x.sheets <- x[inds.sheets]
    x <- x[setdiff(1:length(x), inds.sheets)]
  }
  # Convert sheets to pages
  x.sheets <- sheets2pages(x.sheets)
  x.sheets <- sum(na.omit(suppressWarnings(as.numeric(x.sheets))))

  # Identify square bracket positions
  inds <- grep("\\[", x)  
  nonsq.inds <- 1:length(x)
  x.sq <- 0
  if (length(inds) > 0) {  
    nonsq.inds <- setdiff(nonsq.inds, inds)
    x.sq <- x[inds]
    x.sq <- gsub("sheets", "", x.sq)
    x.sq <- gsub("sheet", "", x.sq)
    x.sq <- suppressWarnings(as.numeric(str_trim(gsub("\\]", " ", gsub("\\[", " ", x.sq)))))

    # If dashes associated with square brackets, consider them arabic
    # [3]-14 -> 3-14 -> 14
    if (length(grep("-",x.sq))>0) {
      sinds <- grep("-",x.sq)
      x3 <- x.sq[sinds]
      x.sq <- x.sq[setdiff(1:length(x.sq), sinds)]
      if (length(x.sq)==0) { x.sq <- 0 }
      x <- c(x, x3)
    }  
    x.sq <- x.sq[!as.character(x.sq) == "0"]
  }
  x.nonsq <- x[nonsq.inds]

  # Romans in the beginning
  romans.start <- 0
  romans.start.inds <- which.min(sapply(x.nonsq, is.roman)) - 1

  if (romans.start.inds > 0) {
    romans.start.inds <- 1:romans.start.inds
    romans.start <- max(sapply(unlist(strsplit(x.nonsq[romans.start.inds], "-")), function (x) {as.numeric(as.roman(x))}))
    # Pick remaining arabics
    x.arabic <- all2arabics(x.nonsq[-romans.start.inds])
  } else {
    # Pick remaining arabics
    x.arabic <- all2arabics(x.nonsq)
  }
  if (is.na(romans.start) || length(romans.start) == 0) {romans.start <- 0}

  # print(c(roman = romans.start, arab = x.arabic, sheet = x.sheets, sq = x.sq))

  # Convert square brackets into arabics
  if (length(x.sq)>0) {
    x.squarebrackets <- sum(na.omit(suppressWarnings(as.numeric(sapply(x.sq, function (x) {as.roman(x)})))))
  } else {
    x.squarebrackets <- 0
  }

  # Check if the non-squarebracket series is increasing
  arabics <- x.arabic

  increasing <- is.increasing(x.arabic)
  is.series <- length(grep("-", x.nonsq))>0 
  x.pagecount <- romans.start + x.sheets 

  if (!is.series) {
    # Not a series
    if (increasing) {
      x.pagecount <- sum(na.omit(c(romans.start, x.arabic, sum(x.sq), x.sheets)))
    } else {
      x.pagecount <- max(na.omit(c(romans.start, x.arabic, x.sheets)))
    }

  } else if (increasing) {

    # series; increasing
    x.pagecount <- romans.start + max(arabics) - min(arabics) + 1 + x.sheets

    if (length(inds) > 0) {

      # Square brackets present
      # Last square brackets after the other numbers
      last.sq <- 0
      n <- min(length(inds), length(x))
      # lastinds <- na.omit(rev(inds)[which(rev(inds)[1:n] == rev(seq(length(x)))[1:n])]) # last square bracket occurrences
      lastinds <- unique(na.omit(inds)) # all square bracket occurences

      if (length(lastinds) > 0) { 
        last.sq <- x[lastinds]
      	last.sq <- str_trim(gsub("\\]", " ", gsub("\\[", " ", last.sq)))
      	last.sq[last.sq == ""] <- NA
      	last.sq <- as.numeric(na.omit(last.sq))

	if (length(arabics) == 1) {
	  x.arab <- arabics
	} else {
	  x.arab <- max(arabics) - min(arabics) + 1
	}

	tmp <- c(roman = romans.start, arabics = x.arab, sqb = last.sq, sheet = x.sheets)

	x.pagecount <- sum(na.omit(tmp))

      } else {
        x.pagecount <- romans.start + max(arabics) + x.sheets
      }
    }

  } else if (!increasing) {
    # series; not increasing
    x.pagecount <- sum(na.omit(c(romans.start, max(x.arabic), sum(x.sq), x.sheets)))
  } 


  # Convert romans to arabics (entries separated by spaces possibly)
  #li <- lapply(x, function (x) { pages2arabics(x) })
  #x.arabic <- unname(unlist(sapply(li, function (x) {x$arabic})))
  #x.roman <-  unname(unlist(sapply(li, function (x) {x$roman})))
  #if (length(x.arabic) == 0 || is.null(x.arabic) || is.na(x.arabic)) { x.arabic <- 0 }
  # Handle potential double pages
  #x.arabic <- double_pages(x.arabic)
  #x.roman <- double_pages(x.roman)
  # Put together pages from square brackets and otherwise
  #res <- c(arabic = as.numeric(x.arabic), 
  #         roman = as.numeric(x.roman), #
  #	   squarebrackets = as.numeric(x.squarebrackets), 
  #	   plate.pages = as.numeric(x.plates))
  # Add total page count
  #res[["total"]] <- sum(na.omit(suppressWarnings(as.numeric(res))))

  # If total page count is 0, then mark it as NA
  if (x.pagecount == 0) {x.pagecount <- NA}

  x.pagecount

}


sheets2pages <- function (x) {

  sheets2pages.single <- function (x) {
    str_trim(unlist(strsplit(x, "sheet"))[[1]]) 
  }

  inds <- grep("sheet", x)
  if (length(inds) > 0) {
    # 1 sheet = 2 pages
    pages <- sapply(x[inds], function (x) {sheets2pages.single(x)})
    pages <- as.numeric(as.roman(pages))
    x[inds] <- 2*pages
  }

  x

}


double_pages <- function (x) {

  if (length(x) == 0) {return (0)}	     

  # x is a vector of page number strings
  # If some pages are separated by "-" then just pick the maximum page number
  x <- as.numeric(unlist(strsplit(x, "-")))
  x <- str_trim(x)
  x <- as.numeric(as.vector(na.omit(x)))

  max(x)

}


plates2pages <- function (s) {

  # TODO these harmonization functions could go here from 
  # harmonize_pages
  s <- polish_ie(s) 

  # If not plate number given, consider it as a single plate
  # Convert plates to pages: 1 plate = 2 pages
  if (length(grep("plate", s)) > 0 || length(grep("lea", s)) > 0) {
    if (length(grep("plates", s) > 0) && length(grep("lea", s)) == 0) {
      # "plates" instances without "leaf" or "leaves"
      xi <- str_trim(gsub("plates", "", s))
      xi <- gsub("\\]", "", gsub("\\[", "", xi))
      # When no plate number is given, use plates = 2 plates
      xi[xi == ""] <- 2
      s <- as.numeric(as.roman(xi))
    } else if (length(grep("plate", s) > 0) && length(grep("lea", s)) == 0) {
      # "plate" instances without "leaf" or "leaves"
      xi <- str_trim(gsub("plate", "", s))
      xi <- gsub("\\]", "", gsub("\\[", "", xi))
      # When no plate number is given, use 1 (plate = 1 page)
      xi[xi == ""] <- 1
      s <- as.numeric(xi)
    } else if (length(grep("leaf", s)) > 0) {
      # "leaf" instances 
      xi <- str_trim(gsub("leaf", "", s))
      xi <- gsub("\\]", "", gsub("\\[", "", xi))
      xi[xi == ""] <- 1
      # When no leaf number is given, use 1 (1 leaf)
      xi <- as.numeric(xi)
      # multiply the numbers by 2 (1 leaf = 2 pages)
      s <- 2 * xi
    } else if (length(grep("leaves", s)) > 0) {
      # "leaves" instances 
      xi <- str_trim(gsub("leaves", "", s))   
      xi <- gsub("\\]", "", gsub("\\[", "", xi))
      # When no leaf number is given, use 2 (2 leaves)
      xi[xi == ""] <- 2
      s <- as.numeric(sapply(xi, function (x) {as.roman(x)}))
    }

    # multiply the numbers xi by 2 (4 leaves = 8 pages)
    s <- 2 * s
    s <- paste(s, "pages calculated from plates")
  }

  s

}
