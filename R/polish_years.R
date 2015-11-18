#' @title polish_years
#' @description Pick and polish the year interval (start and end years) from a time field which is of the form 1800 or 1823-1845 etc.
#' @param x year field (a vector) 
#' @param start_synonyms Synonyme table for start year
#' @param end_synonyms Synonyme table for end year
#' @param verbose verbose
#' @param check If true, remove entries (replace by NA) where start > end
#' @return data.frame with the fields 'start' and 'end'
#' @export
#' @author Leo Lahti and Niko Ilomaki \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_years(c("1746", "1745-1750"))}
#' @keywords utilities
polish_years <- function(x, start_synonyms=NULL, end_synonyms=NULL, verbose = TRUE, check = FALSE) {

  xorig <- x <- as.character(x)
  
  xuniq <- unique(x)
  match.inds <- match(x, xuniq)  
  if (verbose) {
    message(paste("Polishing years:", length(xuniq), "unique cases"))
  }

  x <- xuniq

  # "Printed in the Yeare,;1648."
  inds <- grep(";", x)
  x[inds] <- sapply(x[inds], function (x) {x <- unlist(strsplit(x, ";")); paste(x[grep("[0-9]", x)], collapse = ", ")})

  x <- harmonize_ie(x)

  x <- remove_print_statements(x)

  tab <- suppressWarnings(t(sapply(x, function (x) {polish_year(x, start_synonyms = start_synonyms, end_synonyms = end_synonyms)})))

  start_year <- unname(tab[, "from"])
  end_year <- unname(tab[, "till"])  

  # For now, give end years for all entries for compatibility reasons
  # TODO: later allow NAs  
  #inds <- which(is.na(end_year))
  #end_year[inds] <- start_year[inds]

  # Match the unique cases to the original indices
  xorig <- xorig  # this is already in the original index domain
  start_year <- start_year[match.inds]
  end_year <- end_year[match.inds]

  if (check) {
    inds <- which(start_year > end_year)
    if (length(inds) > 0) {
      start_year[inds] <- NA
      end_year[inds] <- NA      
    }
  }

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

  # x [i.e y] -> y
  x <- try(x <- handle_ie(x, harmonize = TRUE))
  if (class(x) == "try-error") { x <- NA }

  x <- condense_spaces(gsub("\\.", " ", x))

  x <- remove_time_info(x)

  inds <- grep(" or ", x)
  if (length(inds)>0) {
    x[inds] <- sapply(x[inds], function (x) str_trim(unlist(strsplit(x, " or "))[[2]]  ))
  }

  # Remove some other info
  x <- gsub("price [0-9] d", "", x)
  x <- gsub("-[0-9]{2,3}\\?{1,2}$", "", x)
  x <- gsub("\\?", "", x)

  # Convert romans
  num <- as.numeric(as.roman(gsub("\\,", "", gsub(" ", "", x))))
  if (!is.na(num)) {
    x <- num
  }

  # "1646[7]" -> 1646
  if (length(grep("^[0-9]{4}\\[[0-9]\\]", x))>0) {
    x <- substr(x, 1, 4)
  } else if (length(grep("^[0-9]{4}-\\[[0-9]*\\]", x))>0) {
    # "1646-[27]" -> 1646-27
    x <- gsub("\\[", "", gsub("\\]", "", x))
  }  

  # Remove all letters
  x <- gsub("[a-z]+-[a-z]+", " ", x)
  x <- gsub("[a-z|A-Z]", "", x)
  x <- condense_spaces(x)
  if (length(x) > 1) {
    x <- na.omit(x)
  }

  # Remove some special chars
  x <- gsub("\\(\\)", "", x)  
  x <- gsub("-+", "-", x)
  x <- gsub("b\\.c\\.", "before christian era", x)
  x <- gsub("^<*", "", gsub(">$", "", x))
  x <- gsub("<*", "", gsub(">", "", x))  
  x <- gsub(" - ", "-", x)
  x <- condense_spaces(x)
  x <- gsub("\\,\\,", ",", x)  
  x <- gsub("^\\, ", "", x)
  x <- gsub("\\{", " ", gsub("\\}", " ", x))
  x <- gsub("\\\\>", " ", x)

  if (length(grep("[0-9]{2}\\[[0-9]{2}\\]", x))>0) {
    x <- gsub("\\[", "", gsub("\\]", "", x))      
  } else if (length(grep("[0-9]{3}\\[[0-9]{1}\\]", x))>0) {
    x <- gsub("\\[", "", gsub("\\]", "", x))      
  }

  # 1695/6 [1696]
  if (length(grep("^[0-9]{4}/[0-9] \\[[0-9]{4}\\]", x))>0) {
    x <- gsub("\\]", "", unlist(strsplit(x, "\\["))[[2]])
  }

  # 1726[1727] -> 1727
  if (length(grep("^[0-9]{4}\\[[0-9]{4}\\]", x))>0) {
    x <- paste(substr(x, 6, 9), substr(x, 11, nchar(x)), sep = " ")
  }
  # 1726 [1727]  -> 1726 or 1736[1735]-38 -> 1735-38
  if (length(grep("^[0-9]{4}[ |]\\[[0-9]{4}\\]", x))>0) {
    x <- gsub(" \\[", "[", x)
    x <- paste(substr(x, 1, 4), substr(x, 11, nchar(x)), sep = "")
  }
  x <- gsub("\\[[0-9]{2,3}-*\\]", "", x)  

  x <- condense_spaces(gsub("\\[", " [", x))
  x <- gsub("-\\]-", "-", x)
  x <- gsub("- ", "-", x)

  x <- gsub("\\[[0-9]{2}-\\]", "NA", x)
  x <- gsub("\\[[0-9]{2}-\\?", "NA", x)
  x <- gsub("\\[[0-9]{2}\\?\\?", "NA", x)
  x <- gsub("\\[[0-9]{3}-\\?", "NA", x)  

  x <- gsub("-[0-9]{2}-\\]", "-NA", x)
  x <- gsub("-[0-9]{2}-\\?", "-NA", x)
  x <- gsub("-[0-9]{2}\\?\\?", "-NA", x)

  x <- gsub("\\[", " ", gsub("\\]", " ", x))  
  x <- condense_spaces(x)
  x <- gsub("^\\(", "", gsub("\\)$", "", x))

  # Mark missing year with NA
  x <- gsub("^-", "NA-", gsub("-$", "-NA", x))
  x <- gsub(" -", "-", gsub("- ", "-", x))
  x <- gsub("^\\:", "", x)
  x <- gsub("^\\,", "", x)
  x <- gsub("^\\=", "", x)        

  # Harmonize missing years
  if (is.na(x) || x == "") {x <- "NA-NA"}

  # NA-1524-NA

  # NA-1524-1700
  if (length(grep("NA-[0-9]{4}-[0-9]{4}$", x))>0) {
    x <- substr(x, 4, 13)
  }

  # 1736-49
  if (length(grep("NA-[0-9]{4}-NA$", x))>0) {
    x <- substr(x, 4, 8)
  }

  # 1736-49
  if (length(grep("[0-9]{4}-[0-9]{1,2}$", x))>0) {
    spl <- unlist(strsplit(x, "-"))
    x <- paste(spl[[1]], paste(substr(spl[[1]], 1, 4-nchar(spl[[2]])), spl[[2]], sep = ""), sep = "-")
  }

  # 1736/49
  x <- condense_spaces(x)
  if (length(grep("[0-9]{4}/[0-9]*$", x))>0) {
    x <- substr(x, 1, 4)
  }

  # 1780 1800
  if (length(grep("[0-9]{4} [0-9]*$", x))>0) {
    x <- gsub(" ", "-", x)
  }

  # "( ) 25 1643"
  spl <- unlist(strsplit(x, " "))
  spl <- unique(spl[grep("[0-9]{4}", spl)])
  if (length(spl) == 1) {x <- spl}
  x <- gsub("\\,$", "", gsub("\\[", "", gsub("\\]", "", x)))

  # 1690, 1690
  if (length(grep("[0-9]{4}, [0-9]{4}$", x))>0) {  
    x <- as.character(min(as.numeric(unique(unlist(strsplit(x, ","))))))
  }


  # Proceed to more complex cases
  start <- harmonize_names(x, start_synonyms)$name
  start <- as.character(start)

  spl <- unlist(strsplit(as.character(start), "-"))
  spl <- as.numeric(spl)  
  if (sum(is.na(spl))>1) {
    # NA, 3, NA -> 3, NA (assume the year is start year if it is not clear)
    spl <- spl[min(which(!is.na(c(NA, 3, NA)))):length(spl)]
  }
  x <- spl

  start <- x[[1]]
  if (length(x) > 1) {
    end <- x[[2]]
  } else {
    end <- NA
  }
  x <- paste(x, collapse = "-")

  # A couple of quick exits for the impatient
  # 1900
  if (length(grep("^[0-9]{4}$", x)) > 0) {
    start <- gsub("^([0-9]+)$", "\\1", x)
    end <- NA
    return (c(from=as.numeric(start), till=end))
  } else if (length(grep("^[0-9]{4}[-][0-9]{4}$", x)) > 0) {
    # 1900-1910
    start <- gsub("^([0-9]{4}).*", "\\1", x)
    end <- gsub(".*([0-9]{4})$", "\\1", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))
  } else if (length(grep("^NA[-][0-9]{4}$", x)) > 0) {
    # NA-1910
    start <- NA
    end <- gsub("NA-", "", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))    
  } else if (length(grep("^[0-9]{4}[-]NA$", x)) > 0) {
    # 1900-NA
    start <- gsub("-NA", "", x)
    end <- NA
    return (c(from=as.numeric(start), till=as.numeric(end)))        
  } else if (length(grep("\\[[0-9]*\\]", x)) > 0) {
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
  }

  start <- start[!start %in% c("", " ")]
  start <- christian2numeric(start) 
  start_year <- as.numeric(start)

  end <- harmonize_names(end, end_synonyms)$name
  end <- as.character(end)

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
  x <- gsub("before christian era", "B.C", x)  

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