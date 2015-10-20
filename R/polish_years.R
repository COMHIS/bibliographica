#' @title polish_years
#'
#' @description Pick and polish the year interval (start and end
#  years) from a time field which is of the form 1800 or 1823-1845 etc.
#'
#' @param x year field (a vector) 
#' @param start_synonyms Synonyme table for start year
#' @param end_synonyms Synonyme table for end year
#' @return data.frame with the fields 'start' and 'end'
#'
#' @export
#' 
#' @author Leo Lahti and Niko Ilomaki \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{df <- polish_years(c("1746", "1745-1750"))}
#' @keywords utilities
polish_years <- function(x, start_synonyms=NULL, end_synonyms=NULL) {

  xorig <- x <- as.character(x)
  x <- remove_print_statements(x)

    tab <- t(sapply(x, function (x) {polish_year(x, start_synonyms = start_synonyms, end_synonyms = end_synonyms)}))
    start_year <- unname(tab[, "from"])
    end_year <- unname(tab[, "till"])  

    # For now, give end years for all entries for compatibility reasons
    # TODO: later allow NAs 
    inds <- which(is.na(end_year))
    end_year[inds] <- start_year[inds]
    
  data.frame(list(original = xorig, from = start_year, till = end_year))
}



#' @title polish_year
#'
#' @description Pick and polish the year interval (start and end
#  years) from a time field '
#' @param x year string
#' @param start_synonyms Synonyme table for start year
#' @param end_synonyms Synonyme table for end year
#' @return vector with the fields 'from' and 'till'
#'
#' 
#' @author Leo Lahti and Niko Ilomaki \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{df <- polish_year(c("1746"))}
#' @keywords utilities
polish_year <- function(x, start_synonyms = NULL, end_synonyms = NULL) {

  # TODO: rewrite the function to clarify the logics. See the unit tests.

  if (is.na(x)) {
    return(c(from = NA, till = NA))
  }

  if (is.null(start_synonyms)) {
    f <- system.file("extdata/fi_start_years.csv", package = "bibliographica")
    start_synonyms <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
  }

  if (is.null(end_synonyms)) {
    f <- system.file("extdata/fi_end_years.csv", package = "bibliographica")
    end_synonyms <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
  }
  
  xorig <- x
  x <- remove_endings(x, "\\.")
  x <- gsub("]]", "]", x)  
  x <- remove_terms(x, c("ca\\.", "c\\.", "anno dom", "a.d", "anno domini", "active in", "active", "approximately"), "full")
  x <- gsub("^-*", "", gsub("-*$", "", x))
  x <- gsub("^<*", "", gsub(">$", "", x))
  x <- harmonize_ie(x)
  x <- gsub(" - ", "-", x)

  if (length(grep("-", x))==0 & nchar(gsub("[^0-9]", "", x)) == 4) {
    x <- gsub("[^0-9]", "", x)
  }

  x <- try(handle_ie(x))
  if (class(x) == "try-error") { x <- NA }

  start <- harmonize_names(x, start_synonyms)$name
  start <- as.character(start)

  spl <- strsplit(as.character(start), "-")

  if (length(grep("\\[[0-9]*\\]", x))>0) {
    x <- gsub("\\[", "", gsub("\\]", "", x))
  } else if (length(grep("\\[[0-9]*\\?\\]", x))>0) {
    x <- gsub("\\[", "", gsub("\\?\\]", "", x))
  } else if (length(grep("\\[[0-9]*-[0-9]*\\]", x))>0) {
    ##[1721-1726]  
    x <- gsub("\\[", "", gsub("\\]", "", x))  
  }

  start <- x
  end <- x

  if (length(grep("\\[[0-9]*\\]", x)) > 0) {
    # MDCCLXVIII. [1768]  
    spl <- unlist(strsplit(x, " "))
    if (length(spl) > 1) {spl <- spl[[2]]} else {spl <- spl[[1]]}
    start <- gsub("\\[", "", gsub("\\]", "", spl))
    end <- NA    
  } else if (length(grep("^[0-9]*\\.", x)) > 0) {
    # "1798. (price one dollar)"  
    start <- unlist(strsplit(x, "\\."))[[1]]
    end <- NA
  } else if (length(grep("\\[between [0-9]* and [0-9]*\\?\\]", x))>0 || length(grep("\\[between [0-9]* and [0-9]*\\]", x))>0 || length(grep("between [0-9]* and [0-9]*", x))>0) {
    # [between 1790 and 1800?]  
    spl <- unlist(strsplit(x, " "))
    start <- spl[[2]]
    end <- gsub("\\?$", "", gsub("\\]$", "", spl[[4]]))
  } else if (length(grep("^\\[[0-9]* or [0-9]*\\]", x)) > 0) {
    # [1 or 2]  
    start <- unlist(strsplit(gsub("\\[", "", x), "or"))[[1]]
    end <- NA    
  } else if (length(grep("^\\[[0-9]*\\.\\]", x)) > 0) {
    # [1768.]  
    start <- gsub("\\[","",gsub("\\.\\]","",x))
    end <- NA    
  } else if (length(grep("^\\[ -[0-9]*\\]", x)) > 0) {  
    # [ -1727]
    start <- gsub("\\[ -", "", gsub("\\]", "", x))
    end <- NA
  } else if (length(grep("^\\[ [0-9]*\\]", x)) > 0) {
    # [ 1727]
    start <- gsub("\\[ ", "", gsub("\\]", "", x))
    end <- NA    
  } else if (length(grep("^[0-9]*\\]", x)) > 0) {
    # 1768]  
    start <- gsub("\\]","",x)
    end <- NA
  } else if (length(grep("^[0-9]*\\?\\]", x)) > 0) {
    # 1768?]  
    start <- gsub("\\?\\]","",x)
    end <- NA
  } else if (length(grep("^[0-9]*\\.\\]", x)) > 0) {
    # 1768.]  
    start <- gsub("\\.\\]","",x)
    end <- NA    
  } else if (length(grep("*\\[[0-9]*\\]", x)) > 0) {
    # MDCCLXVIII[1768]  
    spl <- gsub("\\]", "", unlist(strsplit(x, "\\["))[[2]])
    start <- spl
    end <- NA    
  } else if (length(grep("[0-9]*", x)) > 0) {
    # MDCCLXVIII. 1768  
    spl <- unlist(strsplit(x, " "))
    if (length(spl) > 1) {spl <- spl[[2]]} else {spl <- spl[[1]]}
    start <- spl
    end <- NA
  } else if (length(grep("^\\[[0-9]*\\]", x)) > 0) {
    # [1768]  
    start <- gsub("\\[","",gsub("\\]","",x))
    end <- NA
  } else if (length(grep("^\\[[0-9]*", x)) > 0) {
    # [1727-  
    start <- gsub("\\[", "", x)
    end <- NA
  }

  spl <- strsplit(start, "-")

  start <- sapply(spl, function (x) {if (length(x) >= 1) {x[[1]]}})

  inds <- grep("^-", start)
  if (length(inds)>0) {
    start[inds] <- NA
  }

  start <- gsub("\\?$", "", start)
  start <- gsub("\\?\\.$", "", start)  
  inds <- grep(" or ", start)
  if (length(inds)>0) {
    start[inds] <- sapply(start[inds], function (x) str_trim(unlist(strsplit(x, " or "))[[2]]  ))
  }

  start <- start[!start %in% c("", " ")]
  start <- christian2numeric(start) 
  start_year <- as.numeric(start)

  end <- x  
  end <- harmonize_names(x, end_synonyms)$name
  end <- as.character(end)

  inds <- grep("-", end)
  if (length(inds)>0) {
    end[inds] <- sapply(end[inds], function (x) {spl <- unlist(strsplit(as.character(x), "-")); if (length(spl) > 1) {spl[[2]]}})
  }

  end <- gsub("\\?$", "", end)    
  end <- christian2numeric(end)   
  end_year <- as.numeric(end)

  if (length(start_year) == 0) {start_year <- NA}
  if (length(end_year) == 0) {end_year <- NA}  

  if (length(start_year) > 1) {start_year <- NA}
  if (length(end_year) > 1) {end_year <- NA}

  c(from = start_year, till = end_year)

}


christian2numeric <- function (x) {

  x <- str_trim(as.character(x))
  x <- gsub("anno dom.", "A.D", x)
  x <- gsub("an. dom.", "A.D", x)  
  x <- gsub("anno domini", "A.D", x)    
  x <- gsub("a.d.", "A.D", x)
  x <- gsub("b.c.", "B.C", x)

  inds <- grep("A.D", x)
  if (length(inds) > 0) {
    x[inds] <- as.numeric(str_trim(gsub("A.D", "", x[inds])))
  }

  inds <- grep("B.C", x)
  if (length(inds) > 0) {
    x[inds] <- -as.numeric(str_trim(gsub("B.C", "", x[inds])))
  }

  x

}