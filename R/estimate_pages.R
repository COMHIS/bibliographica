estimate_pages <- function (x) {

  # Handle the straightforward standard cases first
  if (all(is.na(x))) {
    # "NA"
    return(NA)
  } else if (length(grep("^plates,plates*$", x))) {
    return(NA)  
  } else if (!is.na(suppressWarnings(as.numeric(x)))) {
    # "3"
    return(as.numeric(x))
  } else if ((is.roman(x) && length(unlist(strsplit(x, ","), use.names = FALSE)) == 1 && length(grep("-", x)) == 0)) {

    # "III" but not "ccclxxiii-ccclxxiv"
    return(suppressWarnings(as.numeric(as.roman(x))))
  } else if (length(grep("^\\[[0-9]+ {0,1}[p|s]{0,1}\\]$", x)>0)) {

    # "[3]" or [3 p]
    return(as.numeric(str_trim(gsub("\\[", "", gsub("\\]", "", gsub(" [p|s]", "", x))))))
  } else if (length(grep("^[0-9]+ sheets$", x)) == 1) {
    # "1 sheet is 2 pages"
    return(2 * as.numeric(as.roman(str_trim(unlist(strsplit(x, "sheet"), use.names = FALSE)[[1]])))) 
  } else if (length(grep("\\[{0,1}[0-9]* \\]{0,1} leaves", x)) > 0) {
    # "[50 ] leaves"    
    x <- str_trim(gsub("\\[", "", gsub("\\]", "", x)))
  } else if (length(grep("[0-9]+ \\+ [0-9]+", x))>0) {
    # 9 + 15
    return(sum(as.numeric(str_trim(unlist(strsplit(x, "\\+"), use.names = FALSE)))))
  } else if (!is.na(sum(as.numeric(roman2arabic(str_trim(unlist(strsplit(x, "\\+"), use.names = FALSE))))))) {
    # IX + 313
    return(sum(as.numeric(roman2arabic(str_trim(unlist(strsplit(x, "\\+"), use.names = FALSE))))))
  } else if (length(grep("^p", x)) > 0 && length(grep("-", x)) == 0) {
    # p66 -> 1
    if (is.numeric(str_trim(gsub("^p", "", x)))) {
      return(1)
    } else if (length(grep("^p", x)) > 0 && length(grep("-", x)) > 0) {
      # p5-8 -> 5-8
      x <- gsub("^p", "", x)
    }    
  } else if (length(grep("^1 sheet \\[*[0-9+]\\]*", x))>0) {
    # "1 sheet [166]"
    x <- gsub("1 sheet", "", x)
    # 1 sheet ([1+] p.)
    x <- gsub("\\[1\\]", "2", x)
  } else if (length(grep("^[0-9]+ sheets* [0-9]+ pages*$", x))>0) {
    # 3 sheets 3 pages
    x <- gsub("^s", "", unlist(strsplit(x, "sheet"), use.names = FALSE)[[2]])
  }

  # --------------------------------------------

  # Then proceeding to the more complex cases...
  # Harmonize the items within commas

  # Remove plus now
  x <- gsub("\\+", "", x)
  x <- gsub("pages*$", "", x)  

  # "[52] plates between [58] blank sheets"
  x <- gsub("plates between ", "plates, ", x)
  # 6 sheets + 2 sheets
  x <- gsub("sheets", "sheets,", x)

  # Handle comma-separated elements separately
  spl <- condense_spaces(unlist(strsplit(x, ","), use.names = FALSE))

  # 13 [1] -> 13, [1]
  if (length(grep("^[0-9]+ \\[[0-9]+\\]$", spl))>0) {
    spl <- gsub(" ", ", ", spl)
  }  
  spl <- condense_spaces(unlist(strsplit(spl, ","), use.names = FALSE))

  # Harmonize pages within each comma
  x <- sapply(spl, function (x) { harmonize_pages_by_comma(x) }, USE.NAMES = FALSE)

  # Remove empty items
  x <- as.vector(na.omit(x))
  if (length(x) == 0) {x <- ""}

  page.count.multiplier <- 1
  if (length(grep("^ff", x[[1]]))==1) {
    # Document is folios - double the page count!
    page.count.multiplier <- 2
  }

  # Fix romans
  x[x == "vj"] <- "vi"

  # Identify (potentially overlapping) attribute positions for
  # "arabic", "roman", "squarebracket", "dash", "sheet", "plate"
  # attributes x positions table 0/1
  # NOTE this has to come after harmonize_per_comma function ! 
  pagecount.attributes <- attribute_table(x)

  # If dashes are associated with square brackets, 
  # consider and convert them to arabic. Otherwise not.
  # ie. [3]-5 becomes 3-5 
  dash <- pagecount.attributes["dash", ]
  sqb  <- pagecount.attributes["squarebracket", ]
  inds <- which(dash & sqb)
  pagecount.attributes["arabic", inds] <- TRUE
  pagecount.attributes["squarebracket", inds] <- FALSE

  # Page count can't be roman and arabic at the same time.
  # or pages will double
  pagecount.attributes["roman", pagecount.attributes["arabic", ]] <- FALSE

  # Remove square brackets
  x <- gsub("\\[", "", x)
  x <- gsub("\\]", "", x)

  # Convert romans to arabics (entries separated by spaces possibly)
  # also 3-iv -> 3-4 
  inds <- pagecount.attributes["roman", ] | pagecount.attributes["arabic", ]
  if (any(inds)) {
    x[inds] <- roman2arabic(x[inds])
  }

  # Convert plates to arabics
  inds <- pagecount.attributes["plate", ]
  if (any(inds)) {  
    x[inds] <- as.numeric(str_trim(gsub("pages calculated from plates", "", x[inds])))
  }

  # ----------------------------------------------

  # Start page counting
  pages <- c()

  # Sum square brackets: note the sum rule does not concern roman numerals
  inds <- pagecount.attributes["squarebracket",] & !pagecount.attributes["roman",]
  pages$squarebracket <- sumrule(x[inds])

  # Sum plates 
  # FIXME: at the moment these all go to sheets already
  inds <- pagecount.attributes["plate",]
  pages$plate <- sum(na.omit(suppressWarnings(as.numeric(x[inds]))))
  
  # For now, always ignore plates. THis helps us to easily recognize
  # documents where page count info is missing (except plates)
  # pages$plate <- 0 # Ignored earlier, only in cases where no other page info
  # such as "2v., plates".

  # Count pages according to the type
  for (type in c("arabic", "roman")) {
    pages[[type]] <- count_pages(x[pagecount.attributes[type,]])
  }

  # Sum sheets 
  inds <- pagecount.attributes["sheet",]
  xx <- NA
  xinds <- x[inds]
  xinds <- gsub("^sheets*$", "1 sheet", xinds)

  if (length(grep("sheet", xinds))>0) {
    # 1 sheet = 2 pages
    xinds <- sapply(xinds, function (xi) {str_trim(unlist(strsplit(xi, "sheet"), use.names = FALSE)[[1]])}, USE.NAMES = FALSE)
    xx <- suppressWarnings(2 * as.numeric(as.roman(xinds)))
  } 
  pages$sheet <- sumrule(xx) 

  # If nothing else than plate info is given then consider this an
  # empty document with no pages. Except when plate count is higher than 2.
  nonzero <- names(which(pages > 0))
  if (length(nonzero) == 1 && nonzero == "plate" && pages$plate <= 4) {
    pages <- 0
  }

  # Take into account multiplier
  # (for instance when page string starts with Ff the document is folios
  # and page count will be multiplied by two - in most cases multiplier is 1)
  pages <- page.count.multiplier * unlist(pages, use.names = FALSE)

  # Total page count
  sum(na.omit(pages))

}
