


#' @title polish_years
#'
#' @description Pick and polish the year interval (start and end
#  years) from a time field which is of the form 1800 or 1823-1845 etc.
#'
#' @param x year field (a vector) 
#' @return data.frame with the fields 'start' and 'end'
#'
#' @export
#' 
#' @author Leo Lahti and Niko Ilomaki \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{df <- polish_years(c("1746", "1745-1750"))}
#' @keywords utilities
polish_years <- function(x) {

  xorig <- x <- as.character(x)
  x <- remove_print_statements(x)

    tab <- t(sapply(x, function (x) {polish_year(x)}))
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
#' @return vector with the fields 'from' and 'till'
#'
#' 
#' @author Leo Lahti and Niko Ilomaki \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{df <- polish_year(c("1746"))}
#' @keywords utilities
polish_year <- function(x) {

  # TODO: rewrite the function to clarify the logics. See the unit tests.

  if (is.na(x)) {
    return(c(from = NA, till = NA))
  }

  xorig <- x
  x <- remove_endings(x, "\\.")
  #x <- gsub("^\\[", "", x)
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

  start <- gsub("^([0-9]{3,4})\\D[0-9]{3,4}$","\\1",start)
  start <- gsub("^fl. ([0-9]{3,4})\\D[0-9]{3,4}$",NA,start)
  start <- gsub("^n. ([0-9]{4})\\D[0-9]{4}$","\\1",start)
  start <- gsub("^([0-9]{4})\\Dn. [0-9]{4}$","\\1",start)
  start <- gsub("^n. ([0-9]{4})\\Dn. [0-9]{4}$","\\1",start)
  start <- gsub("^s. ([0-9]{4})$","\\1",start)
  start <- gsub("^s. n. ([0-9]{4})$","\\1",start)
  start <- gsub("^k. ([0-9]{4})$",NA,start)
  start <- gsub("^d. ([0-9]{4})$",NA,start)
  start <- gsub("^k. n. ([0-9]{4})$",NA,start)
  start <- gsub("^k. ennen ([0-9]{4})$",NA,start)
  start <- gsub("^k. viimeistään ([0-9]{4})$",NA,start)
  start <- gsub("^k. ([0-9]{4}) jälkeen$",NA,start)
  start <- gsub("^s. n. ([0-9]{4}), k. [0-9]{4}$","\\1",start)
  start <- gsub("^s. ([0-9]{4}), k. n. [0-9]{4}$","\\1",start)
  start <- gsub("^[0-9]{4}\\Dluku$",NA,start)
  start <- gsub("^eli vielä ([0-9]{4})$",NA,start)
  start <- gsub("^[0-9]$",NA,start)
  start <- gsub("^([0-9]{2,3})\\D[0-9]{2,3} e.Kr$","\\-\\1",start)
  start <- gsub("^n. ([0-9]{2,3})\\D[0-9]{2,3} e.Kr$","\\-\\1",start)
  start <- gsub("^([0-9]{2,3})\\D[0-9]{2,3} e. Kr$","\\-\\1",start)
  start <- gsub("^n. ([0-9]{2,3})\\D[0-9]{2,3} e. Kr$","\\-\\1",start)
  start <- gsub("^s. ehkä 1620-luvulla, k. 1694$",NA,start)
  start <- gsub("^s. 1630-luvulla, k. 1684$",NA,start)
  start <- gsub("^s. 1590-luvulla, k. 1651$",NA,start)
  start <- gsub("^k. 1616/1617$",NA,start)
  start <- gsub("^n. 20 e.Kr.-40 j.Kr$","-20",start)
  start <- gsub("^1600/1700\\-luku$",NA,start)
  start <- gsub("^eli 300\\-luvun puolivälissä$",NA,start)
  start <- gsub("^300-l. j. Kr$",NA,start)
  start <- gsub("^k. 1730-luvulla$",NA,start)
  start <- gsub("^k. vähän ennen vuotta 1600$",NA,start)
  start <- gsub("^n. 363-425 j.Kr$",NA,start)
  start <- gsub("^s. 1678, k. 1695 jälkeen$","1678",start)
  start <- gsub("^s. n. 1560, k. ennen 1617$","1560",start)
  start <- gsub("^s. viim. 1638, k. 1681$",NA,start)
  start <- gsub("^toiminta\\-aika 1770\\-luku$",NA,start)

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

  # remove duplicate handlings
  end <- gsub("^[0-9]{3,4}\\D([0-9]{3,4})$","\\1",end)
  end <- gsub("^fl. [0-9]{3,4}\\D([0-9]{3,4})$",NA,end)
  end <- gsub("^n. [0-9]{4}\\D([0-9]{4})$","\\1",end)
  end <- gsub("^[0-9]{4}\\Dn. ([0-9]{4})$","\\1",end)
  end <- gsub("^n. [0-9]{4}\\Dn. ([0-9]{4})$","\\1",end)
  end <- gsub("^s. ([0-9]{4})$",NA,end)
  end <- gsub("^s. n. ([0-9]{4})$",NA,end)
  end <- gsub("^k. ([0-9]{4})$","\\1",end)
  end <- gsub("^d. ([0-9]{4})$","\\1",end)
  end <- gsub("^k. n. ([0-9]{4})$","\\1",end)
  end <- gsub("^k. ennen ([0-9]{4})$",NA,end)
  end <- gsub("^k. viimeistään ([0-9]{4})$",NA,end)
  end <- gsub("^k. ([0-9]{4}) jälkeen$",NA,end)
  end <- gsub("^s. n. [0-9]{4}, k. ([0-9]{4})$","\\1",end)
  end <- gsub("^s. [0-9]{4}, k. n. ([0-9]{4})$","\\1",end)
  end <- gsub("^[0-9]{4}\\Dluku$",NA,end)
  end <- gsub("^eli vielä ([0-9]{4})$",NA,end)
  end <- gsub("^[0-9]$",NA,end)
  end <- gsub("^[0-9]{2,3}\\D([0-9]{2,3}) e.Kr$","\\-\\1",end)
  end <- gsub("^n. [0-9]{2,3}\\D([0-9]{2,3}) e.Kr$","\\-\\1",end)
  end <- gsub("^[0-9]{2,3}\\D([0-9]{2,3}) e. Kr$","\\-\\1",end)
  end <- gsub("^n. [0-9]{2,3}\\D([0-9]{2,3}) e. Kr$","\\-\\1",end)
  end <- gsub("^s. ehkä 1620-luvulla, k. 1694$","1694",end)
  end <- gsub("^s. 1630-luvulla, k. 1684$","1684",end)
  end <- gsub("^s. 1590-luvulla, k. 1651$","1651",end)
  end <- gsub("^k. 1616/1617$","1616",end)
  end <- gsub("^n. 20 e.Kr.-40 j.Kr$","40",end)
  end <- gsub("^1600/1700\\-luku$",NA,end)
  end <- gsub("^eli 300\\-luvun puolivälissä$",NA,end)
  end <- gsub("^300-l. j. Kr$",NA,end)
  end <- gsub("^k. 1730-luvulla$",NA,end)
  end <- gsub("^k. vähän ennen vuotta 1600$",NA,end)
  end <- gsub("^n. 363-425 j.Kr$",NA,end)
  end <- gsub("^s. 1678, k. 1695 jälkeen$",NA,end)
  end <- gsub("^s. n. 1560, k. ennen 1617$",NA,end)
  end <- gsub("^s. viim. 1638, k. 1681$","1681",end)
  end <- gsub("^toiminta\\-aika 1770\\-luku$",NA,end)

  inds <- grep("-", end)
  if (length(inds)>0) {
    end[inds] <- sapply(end[inds], function (x) {spl <- unlist(strsplit(x, "-")); if (length(spl) > 1) {spl[[2]]}})
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