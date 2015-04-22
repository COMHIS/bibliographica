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
#' @details A summary of page counting rules that this function aims to 
#'          (approximately) implement are provided in 
#' \url{https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html}
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples # polish_pages("4p.")
#' @keywords utilities
polish_pages <- function (s, verbose = FALSE) {

  # Summary of abbreviations
  # http://ac.bslw.com/community/wiki/index.php5/RDA_4.5

  s <- as.character(s)

  # Estimate pages for each document separately via a for loop
  # Vectorization would be faster but we prefer simplicity and modularity here
  raw <- sp <- list()

  for (i in 1:length(s)) {

    if (verbose) { 
      if (ceiling(i/500) == floor(i/500)) {message(i)}
      # {message(i)}
    }

    # Catch warnings rather than crashing the loop
    a <- try(pp <- polish_page(s[[i]]))

    # Save both raw and polished version 
    # We need these later to systematically identify failed cases
    # And to estimate the success fraction
    if ((is.character(a) && a == "try-error") || is.na(pp$pages)) {
      sp[[i]] <- NA
      raw[[i]] <- s[[i]]
    } else {
      tmp <- unname(unlist(pp$pages))
      tmp[is.infinite(tmp)] <- NA
      sp[[i]] <- tmp
      raw[[i]] <- unname(unlist(pp$raw))
    }
  }

  #print("Total page counts") # Calculate sum of parts
  totp <- as.numeric(sapply(sp, function (x) {sum(as.numeric(na.omit(x)))}))
  totp[totp == 0] <- NA # Set zero page counts to NA

  # Collapse page counts
  # sp2 <- sapply(sp, function (s) { paste(s, collapse = ";") })
  list(estimated.pages = sp, raw.pages = raw, total.pages = totp)

}


#' @title polish_page
#' @description Clean up page numbers for a single document.
#'
#' @param x Page number field. Vector or factor of strings.
#' @return Cleaned up version of page number field.
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{polish_page("4p.")}
#' @keywords internal
polish_page <- function (x) {

  # Convert to string 	    	    
  s <- as.character(x)

  # Remove volume info
  # "5v. 3-20, [5]" -> "3-20, [5]"
  s <- remove_volume_info(s)

  # Volumes are separated by semicolons
  # Split by semicolon to list each volume separately
  spl <- str_trim(unlist(strsplit(s, ";")))

  if (length(spl) > 0) {
    # Assess pages per volume
    pages <- sapply(spl, function (x) { estimate_pages(x) })
  } else {
    pages <- NA
  }

  list(raw = spl, pages = pages)
 
}


estimate_pages <- function (x) {

  # Estimate pages for a single volume within a document
  # After removing volume info
  # This is the main function regarding page counting rules	       

  # Harmonize synonymes and spelling errors
  x <- harmonize_pages(x)

  # Handle the straightforward standard cases first
  if (all(is.na(x))) {
    # "NA"
    return(NA)
  } else if (!is.na(suppressWarnings(as.numeric(x)))) {
    # "3"
    return(as.numeric(x))
  } else if ((is.roman(x) && length(unlist(strsplit(x, ","))) == 1 && length(grep("-", x)) == 0)) {
    # "III" but not "ccclxxiii-ccclxxiv"
    return(as.numeric(as.roman(x)))
  } else if (!is.na(suppressWarnings(as.numeric(gsub(" p", "", remove.squarebrackets(x)))))) {
    # "[3]" or [3 p]
    return(as.numeric(remove.squarebrackets(gsub(" p", "", x))))
  } else if (x == "1 sheet") {
    # "1 sheet"
    return(2)
  } else if (length(grep("^[0-9] sheets$", x)) == 1) {
    # "2 sheets"
    return(as.numeric(sheets2pages(x)))
  } 
  # Then proceeding to the more complex cases...

  # --------------------------------------------

  # Harmonize the items within commas
  # Note that the empty items are removed in the end
  # so the length may be shorter
  x <- harmonize_per_comma(x)

  page.count.multiplier <- 1
  if (length(grep("^Ff", x[[1]]))==1) {
    # Document is folios - double the page count!
    page.count.multiplier <- 2
  }

  # -----------------------------------------------------

  # Identify (potentially overlapping) attribute positions for
  # "arabic", "roman", "squarebracket", "dash", "sheet", "plate"
  # attributes x positions table 0/1
  # NOTE this has to come after harmonize_per_comma function ! 
  pagecount.attributes <- attribute_table(x)

  # -----------------------------------------------------

  # If dashes are associated with square brackets, 
  # consider and convert them to arabic  
  # ie. [3]-5 becomes 3-5
  dash <- pagecount.attributes["dash", ]
  sqb  <- pagecount.attributes["squarebracket", ]
  inds <- which(dash & sqb)
  if (length(inds) > 0) {
    x[inds] <- remove.squarebrackets(x[inds])
  }
  pagecount.attributes["arabic", inds] <- TRUE
  # Now page count can't be roman and arabic at the same time.
  # Otherwise pages will calculated double
  pagecount.attributes["roman", pagecount.attributes["arabic", ]] <- FALSE
  pagecount.attributes["squarebracket", inds] <- FALSE

  # -----------------------------------------------------

  # Remove square brackets to simplify later processing
  x <- remove.squarebrackets(x)

  # -----------------------------------------------------

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

  # Sum square brackets
  inds <- pagecount.attributes["squarebracket",]

  pages$squarebracket <- sumrule(x[inds])

  # Sum sheets 
  inds <- pagecount.attributes["sheet",]
  xx <- sheets2pages(x[inds])
  pages$sheet <- sumrule(xx) 

  # Sum plates 
  # FIXME: at the moment these all go to sheets already
  inds <- pagecount.attributes["plate",]
  pages$plate <- sum(na.omit(suppressWarnings(as.numeric(x[inds]))))

  # Count pages according to the type

  for (type in c("arabic", "roman")) {
    xx <- x[pagecount.attributes[type,]]
    pages[[type]] <- count_pages(xx)
  }

  # Convert to vector
  pages <- unlist(pages)

  # Take into account multiplier
  # (for instance when page string starts with Ff the document is folios
  # and page count will be multiplied by two - in most cases multiplier is 1)
  pages <- page.count.multiplier * pages

  # Total page count
  x.pagecount <- sum(na.omit(pages))

  # If total page count is 0, then mark it as NA
  if (x.pagecount == 0) {x.pagecount <- NA}

  x.pagecount

}


sumrule <- function (z) {
  sum(na.omit(suppressWarnings(as.numeric(z))))
}

seqtype <- function (z) {

  # series does not have any
  if (length(z)==0) {
    sequence.type <- "empty"
  } else {

    # Determine page count categories 
    # increasing / series / etc.
    sequence.type <- NA

    # Recognize increasing sequence
    increasing <- is.increasing(z)

    # Recognize series (ie. sequences with dashes)
    series <- length(grep("-", z))>0 

    # Recognize single number

    single.number <- length(z) == 1 && is.numeric(suppressWarnings(as.numeric(z[[1]])) )

    # sequence has numbers without dashes
    if (!series && !increasing) { sequence.type <- "sequence" }
    if (!series && increasing)  { sequence.type <- "increasing.sequence" }
    if (single.number) { sequence.type <- "sequence"}

    # series has dashes
    if (series && !increasing) { sequence.type <- "series" } 
    if (series && increasing) { sequence.type <- "increasing.series" }

    # If there are several arabic elements and at least one dash among them, then use maximum
    multiple <- length(z) > 1
    if (series && multiple) { sequence.type <- "series" }

  }

  sequence.type
}

is.increasing <- function (x) {

  # Ignore square brackets when determining increasing sequence
  #x <- x[!pagecount.attributes["squarebracket",]]

  # Ignore starting romans when determining increasing sequence
  #x <- x[!pagecount.attributes["roman.start",]]

  # Remove dashes
  x <- na.omit(suppressWarnings(as.numeric(unlist(strsplit(x, "-")))))

  # Test if the numeric series is monotonically increasing
  incr <- FALSE
  if (!all(is.na(x))) {
    incr <- all(diff(x) >= 0)
  }

  incr
}


count_pages <- function (z) {

  pp <- 0
  if (length(z) > 0) {
    stype <- seqtype(z)

    if (stype == "increasing.series") {
      pp <- intervalrule(z)
    } else {
      pp <- maxrule(z)
    }
  }
  pp

}

maxrule <- function (x) {
  x <- unlist(strsplit(x, "-"))
  max(na.omit(suppressWarnings(as.numeric(x))))
}

intervalrule <- function (x) {
  x <- unlist(strsplit(x, "-"))
  xx <- na.omit(suppressWarnings(as.numeric(x)))
  max(xx) - min(xx) + 1
}



harmonize_per_comma <- function (x) {

  # Split by comma and handle comma-separated elements as 
  # interpretation units
  spl <- str_trim(unlist(strsplit(x, ",")))

  # Harmonize pages within each comma
  x <- sapply(spl, function (x) { harmonize_pages_by_comma(x) })

  # Remove empty items
  x <- as.vector(na.omit(x))

  x

}

attribute_table <- function (x) {

  # Identify the different page count types and their positions
  # along the page count sequence, including
  # arabics (3), romans ("xiv"), squarebracketed ([3], [3]-5), dashed
  #  (3-5, [3]-5), sheets ("2 sheets"), plates ("plates")
  # NOTE: we allow these types to be overlapping and they are later
  # used to identify the sequence type
  # Initialize attributes vs. positions table
  attributes <- c("arabic", "roman", "squarebracket", "dash", "sheet", "plate")

  pagecount.attributes <- matrix(FALSE, nrow = length(attributes), ncol = length(x))
  rownames(pagecount.attributes) <- attributes

  # ARABIC POSITIONS
  arabics <- position_arabics(x)
  pagecount.attributes["arabic", arabics$positions]<- TRUE

  # ROMAN POSITIONS
  romans <- position_romans(x)
  pagecount.attributes["roman", romans$positions]<- TRUE

  # SQUARE BRACKET POSITIONS
  sqb <- position_squarebrackets(x)
  pagecount.attributes["squarebracket", sqb$positions]<- TRUE

  # DASH POSITIONS
  pagecount.attributes["dash", grep("-", x)]<- TRUE

  # SHEET POSITIONS
  sheets <- position_sheets(x)
  pagecount.attributes["sheet", sheets$positions]<- TRUE

  # PLATE POSITIONS  
  # Estimate pages for plates 
  # and indicate their positions along the page count sequence
  # Example: "127,[1]p.,plates" 
  plates <- position_plates(x) #plates$pages; plates$positions; plates$total
  pagecount.attributes["plate", plates$positions] <- TRUE

  pagecount.attributes

}


position_romans <- function (x) {

  positions <- rep(FALSE, length(x))
  for (i in 1:length(x)) {
    spl <- unlist(strsplit(x[[i]], "-"))
    if (any(sapply(spl, is.roman))) {
      positions[[i]] <- TRUE
    }
  }

  list(positions = positions)

}


position_arabics <- function (x) {

  x <- as.character(x)	     

  positions <- rep(FALSE, length(x))

  for (i in 1:length(x)) {
    spl <- unlist(strsplit(x[[i]], "-"))
    if (any(sapply(spl, function (x) {!is.na(suppressWarnings(as.numeric(x)))}))) {
      positions[[i]] <- TRUE
    }
  }

  list(positions = positions)

}


position_squarebrackets <- function (x) {

  indsa <- sort(which(position_arabics(x)$positions)) # arabics
  indsb <- sort(unique(c(grep("\\[", x), grep("\\]", x)))) # square brackets
  inds <- sort(setdiff(indsb, indsa)) # square brackets that are not of form 91-[93]

  # Indicate positions in the page count sequence
  positions <- rep(FALSE, length(x))
  positions[inds] <- TRUE

  # Calculation of square bracket pages
  # depends on their position and dashes so
  # only indicate position for now
  list(positions = positions)

}

position_sheets <- function (x) {

  # Pick separately pages estimated from sheets
  inds <- grep("sheet", x)

  # Indicate positions in the page count sequence
  positions <- rep(FALSE, length(x))
  positions[inds] <- TRUE

  x.sheets <- 0
  if (length(inds) > 0) { 
    x.sheets <- x[inds]
    x <- x[setdiff(1:length(x), inds)]
  }
  # Convert sheets to pages
  x <- suppressWarnings(as.numeric(sheets2pages(x.sheets)))

  list(pages = x, positions = positions, total = sum(na.omit(x)))


}


position_plates <- function (x) {

  # Pick separately the pages estimated from plates
  inds <- grep("pages calculated from plates", x)
  
  # Indicate plate positions in the page count sequence
  positions <- rep(FALSE, length(x))
  positions[inds] <- TRUE

  x.plates <- 0
  if (length(inds) > 0) { 
    x.plates <- x[inds]
    x <- x[setdiff(1:length(x), inds)]
  }

  x <- suppressWarnings(as.numeric(str_trim(gsub("pages calculated from plates", "", x.plates))))

  # pages: pages calculated from plates separately for each position
  # positions: Positions for plate pages on the page count sequence
  # total: total pages calculated from plates
  list(pages = x, positions = positions, total = sum(na.omit(x)))

}


remove_dimension <- function (s) {

  # Remove commonly used volume formats
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
  s <- gsub("[0-9][0-9]cm\\.", " ", s)

  if (is.na(s) || s == "") {s <- NA}

  s

}




pick_starting_numeric <- function (x) {

  num <- TRUE	
  n <- 0
  i <- 0

  while (!is.na(n) && i < nchar(x)) {
    i <- i+1
    ss <- substr(x, 1, i)
    n <- suppressWarnings(as.numeric(ss))
    if (!is.na(n)) {
      num <-n 
    }
  }

  num

}



polish_ie <- function (x) {

  # 183 i.e 297 -> 297	  
  # 183-285 i.e 297 -> 183-297	  
  # 183-285 [i.e 297] -> 183-297	  

  y <- x

  # Handle ie
  if (length(grep("i\\.e", x)) > 0) {

    x2 <- gsub("\\]", " ", str_trim(unlist(strsplit(x, "i.e")))[[2]])
    yspl <- unlist(strsplit(y, "i\\.e"))
    x0 <- NULL

    if (length(grep("-", yspl[[1]]))>0 && length(grep("-", yspl[[2]]))>0) {

      y <- str_trim(yspl[[2]])

    } else if (length(grep("-", yspl[[1]]))>0) {

      y <- as.numeric(as.roman(str_trim(x2)))
      spl <- str_trim(unlist(strsplit(x, "-")))

      x0 <- as.numeric(as.roman(spl[[1]]))
      x1 <- as.numeric(as.roman(unlist(strsplit(spl[[2]], " "))[[1]]))

      if (x2 > x0) {
        y <- paste(x0, x2, sep = "-")
      } else {
        y <- x2
      }
    } else if (length(grep("-", yspl[[2]]))>0) {

      y <- as.numeric(as.roman(str_trim(x2)))
      # "4 [i.e. 6]-8 p." -> "6-8"
      spl <- str_trim(unlist(strsplit(x, "-")))

      x0 <- str_trim(unlist(strsplit(spl[[1]], "i.e"))[[2]])
      x1 <- as.numeric(unlist(strsplit(spl[[2]], " "))[[1]])

      if (x2 > x0) {
        y <- paste(x0, x2, sep = "-")
      } else {
        y <- x2
      }
    } else {
      xx <- condense_spaces(x2)
      xx <- unlist(strsplit(xx, " "))
      y <- paste(as.numeric(as.roman(xx[[1]])), xx[-1], sep = " ")
    }
  }

  y
}

is.roman <- function (x) {

  x <- gsub("\\.", NA, x)

  check.roman <- function (x) {

    if (x == "" || is.na(x)) {return(FALSE)}

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



roman2arabic <- function (x) {

  for (i in 1:length(x)) {

    xi <- x[[i]]

    if (length(grep("-", xi)) > 0) {
      x2 <- str_trim(unlist(strsplit(xi, "-")))
      n <- suppressWarnings(as.numeric(as.roman(x2)))
      n[is.na(n)] <- x2[is.na(n)] # vii-160
      xr <- paste(n, collapse = "-")
    } else {
      xr <- suppressWarnings(as.numeric(as.roman(xi)))
    }

    x[[i]] <- xr

  }

  x 

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




remove.squarebrackets <- function (x) {
  rm.sqb <- function (x) {		      
    str_trim(gsub("\\[", "", gsub("\\]", "", x)))
  }

  x <- sapply(x, function (x) {rm.sqb(x)})

  x
}




sheets2pages <- function (x) {

  sheets2pages.single <- function (x) {
    if (x == "sheet") {x <- "1 sheet"}
    if (x == "sheets") {x <- "2 sheets"}
    str_trim(unlist(strsplit(x, "sheet"))[[1]]) 
  }

  inds <- grep("sheet", x)
  if (length(inds) > 0) {
    # 1 sheet = 2 pages
    pages <- sapply(x[inds], function (x) {sheets2pages.single(x)})
    pages <- as.numeric(as.roman(pages))
    x[inds] <- 2*pages
  }

  as.numeric(x)

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
  # s <- polish_ie(s) 

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
