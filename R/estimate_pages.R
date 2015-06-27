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
    return(suppressWarnings(as.numeric(as.roman(x))))
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
