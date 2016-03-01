#' @title Polish years
#' @description Pick and polish the year interval (start and end years) from a time field which is of the form 1800 or 1823-1845 etc.
#' @param x year field (a vector) 
#' @param start_synonyms Synonyme table for start year
#' @param end_synonyms Synonyme table for end year
#' @param verbose verbose
#' @param check If true, remove entries (replace by NA) where start > end
#' @return data.frame with the fields 'start' and 'end'
#' @export
#' @importFrom sorvi quickdf
#' @author Leo Lahti and Niko Ilomaki \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_years(c("1746", "1745-1750"))}
#' @keywords utilities
polish_years <- function(x, start_synonyms=NULL, end_synonyms=NULL, verbose = TRUE, check = FALSE) {

  if (is.null(start_synonyms)) {
    f <- system.file("extdata/fi_start_years.csv", package = "bibliographica")
    start_synonyms <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
  }

  if (is.null(end_synonyms)) {
    f <- system.file("extdata/fi_end_years.csv", package = "bibliographica")
    end_synonyms <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
  }

  f <- system.file("extdata/months.csv", package = "bibliographica")
  months <- as.character(read.csv(f, header = TRUE)[,1])
  months <- unique(c(months, tolower(months)))
  # Handle from longest to shortest to avoid problems
  months <- months[rev(order(nchar(months)))]

  xorig <- x <- tolower(as.character(x))
  xuniq <- unique(xorig)
  x <- xuniq

  if (verbose) {
    message(paste("Polishing years:", length(xuniq), "unique cases"))
  }

  # "[1.12.1584 jalkeen]" -> 1584 jalkeen
  inds <- grep("[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}", x)
  x[inds] <- gsub("[0-9]{1,2}\\.[0-9]{1,2}\\.", "", x[inds])

  # 75,9 -> 75.9  
  x <- gsub(",", ".", x)
 
  # 23.1967 -> 1967
  x <- gsub(" [0-9]{1,2}\\.", "", x)   
  
  # "Printed in the Yeare,;1648."
  inds <- grep(";", x)
  x[inds] <- unlist(sapply(x[inds], function (x) {x <- unlist(strsplit(x, ";")); paste(x[grep("[0-9]", x)], collapse = ", ")}), use.names = FALSE)
  x <- gsub("\\.", "", x)

  if (length(grep("-+[a-z]*[0-9]{4}-+", x))>0) {
    x <- gsub("[a-z]", "", x)
    # x <- gsub("[A-Z]", "", x)
    # x <- gsub("-", "", x)        
  }

  # Map back to original indices and make unique again. To speedup further.
  x <- x[match(xorig, xuniq)]
  xorig <- x
  xuniq <- unique(xorig)
  x <- xuniq

  x <- harmonize_ie(x) 
  x <- remove_print_statements(x)

  # Map back to original indices and make unique again. To speedup further.
  x <- x[match(xorig, xuniq)]
  xorig <- x
  xuniq <- unique(xorig)
  x <- xuniq
  
  x <- sapply(x, function (xi) {handle_ie(xi, harmonize = TRUE)})
  x <- condense_spaces(gsub("\\.", " ", x))
  x <- remove_time_info(x, verbose = F, months)

  # Map back to original indices and make unique again. To speedup further.
  x <- x[match(xorig, xuniq)]
  xorig <- x
  xuniq <- unique(xorig)
  x <- xuniq

  # Remove some other info
  x <- gsub("price [0-9] d", "", x)
  x <- gsub("-[0-9]{2,3}\\?{1,2}$", "", x)
  x <- gsub("\\?", "", x)
  x <- gsub("^& ", "", x)  
  x <- gsub(" -", "-", gsub("- ", "-", x))
  x <- harmonize_christian(x)
  
  inds <- grep(" or ", x)
  if (length(inds)>0) {
    x[inds] <- sapply(x[inds], function (x) unlist(strsplit(x, " or "), use.names = FALSE)[[2]])
  }
  x[inds] <- str_trim(x[inds])

  # Convert romans
  num <- as.numeric(as.roman(gsub(" ", "", x)))
  inds <- which(!is.na(num))
  if (length(inds)>0) {
    x[inds] <- num[inds]
  }

  # Map back to original indices and make unique again. To speedup further.
  x <- x[match(xorig, xuniq)]
  xorig <- x
  xuniq <- unique(xorig)
  x <- xuniq
  
  res <- suppressWarnings(lapply(x, function (xi) {a <- try(polish_year(xi, start_synonyms = start_synonyms, end_synonyms = end_synonyms, months, verbose)); if (class(a) == "try-error") {return(c(NA, NA))} else {return(a)}}))

  res <- do.call("rbind", res)
  start_year <- res[,1]
  end_year   <- res[,2]

  if (check) {
    inds <- which(start_year > end_year)
    if (length(inds) > 0) {
      start_year[inds] <- NA
      end_year[inds] <- NA      
    }
  }

  #quickdf(list(from = start_year, till = end_year))
  df <- data.frame(from = start_year, till = end_year)

  # Match the unique cases to the original indices
  # before returning the df
  df[match(xorig, xuniq), ]

}



#' @title Polish year
#' @description Pick and polish the year interval (start and end years) from a time field '
#' @param x year string
#' @param start_synonyms Synonyme table for start year
#' @param end_synonyms Synonyme table for end year
#' @param months months
#' @param verbose verbose
#' @return vector with the fields 'from' and 'till'
#' @author Leo Lahti and Niko Ilomaki \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_year(c("1746"))}
#' @keywords utilities
polish_year <- function(x, start_synonyms = NULL, end_synonyms = NULL, months, verbose = FALSE) {

  # if (verbose) {message(x)}

  # Some quick returns for simple cases to speed up
  if (length(grep("^[0-9]{4}$", x)) > 0) {
    # 1900
    start <- gsub("^([0-9]+)$", "\\1", x)
    end <- NA
    return (c(from=as.numeric(start), till=end))
  } else if (length(grep("^[0-9]{4}[-][0-9]{4}$", x)) > 0) {
    # 1900-1910
    start <- gsub("^([0-9]{4}).*", "\\1", x)
    end <- gsub(".*([0-9]{4})$", "\\1", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))
  }

  # More complex cases..
  # "mdccx-mdccxi [1710-1711]" -> [1710-1711]
  if (length(grep("[a-z]*-[a-z]* \\[[0-9]*-[0-9]*\\]", x))>0) {
    x <- remove_letters(x)
  } else if (length(grep("^[0-9]{4}\\[[0-9]\\]", x))>0) {
    # "1646[7]" -> 1646  
    x <- substr(x, 1, 4)
  } else if (length(grep("^[0-9]{4}-\\[[0-9]*\\]", x))>0) {
    # "1646-[47]" -> 1646-47
    x <- gsub("\\[", "", gsub("\\]", "", x))
  } else if (length(grep("^[0-9]{4}\\[[0-9]{4}\\]-[0-9]{2}", x))>0) {
    # "1646[1647]-50" -> 1647-50
    x <- gsub("\\]", "", substr(x, 6, nchar(x)))
  } else if (length(grep("^[0-9]{4} \\[po [0-9]*\\]", x))>0) {
    # "1738 [po. 1752]" -> 1738
    x <- substr(x, 1, 4)
  }

  # Now Remove some special chars
  x <- gsub("\\(\\)", "", x)  
  x <- gsub("-+", "-", x)
  x <- gsub("^[<|>]*", "", x)
  x <- gsub("[<*|>]", "", x)  
  x <- condense_spaces(x)
  x <- gsub("[\\{|\\}]", " ", x)
  x <- gsub("\\\\>", " ", x)
  x <- gsub("\\[=", "[", x)
  x <- gsub("\\[\\]", "-", x)    

  if (length(grep("[0-9]{2}\\[[0-9]{2}\\]", x))>0) {
    # 19[99]
    x <- gsub("\\[", "", x)
    x <- gsub("\\]", "", x)    
  } else if (length(grep("[0-9]{3}\\[[0-9]{1}\\]", x))>0) {
    # 199[9]
    x <- gsub("\\[", "", x)
    x <- gsub("\\]", "", x)    
  } else if (length(grep("^[0-9]{4}/[0-9] \\[[0-9]{4}\\]", x))>0) {
    # 1695/6 [1696]
    x <- gsub("\\]", "", unlist(strsplit(x, "\\["), use.names = FALSE)[[2]])
  } else if (length(grep("^[0-9]{4} {0,1}\\[[0-9]*\\]", x))>0) {
    # 1726[1727] -> 1727  
    x <- gsub(" \\[", "[", x)
    x <- substr(x, 6, 9)
  }

  x <- gsub("\\[[0-9]{2,3}-*\\]", "", x)  
  x <- gsub("-\\]-", "-", x)
  x <- gsub("\\[[0-9]{2}-\\]", "NA", x)
  x <- gsub("\\[[0-9]{2,3}-\\?", "NA", x)
  x <- gsub("\\[[0-9]{2}\\?+", "NA", x)
  x <- gsub("-[0-9]{2}-\\]", "-NA", x)
  x <- gsub("-[0-9]{2}-\\?", "-NA", x)
  x <- gsub("-[0-9]{2}\\?+", "-NA", x)
  x <- gsub("\\[", " ", x)
  x <- gsub("\\]", " ", x)      
  x <- gsub("^\\(", "", gsub("\\)$", "", x))

  # Mark missing year with NA
  x <- gsub(" -", "-", gsub("- ", "-", x))  
  x <- gsub("^-", "NA-", gsub("-$", "-NA", x))

  # Remove all letters
  x <- gsub("[a-z]", "", x)
  x <- condense_spaces(x)
  x <- gsub("^[\\:|\\=]", "", x)
  if (x == "" || is.na(x)) {x <- "NA"}
  if (length(x) > 1) {
    x <- na.omit(x)
  }

  # NA-1524-1700
  if (length(grep("NA-[0-9]{4}-[0-9]{4}$", x))>0) {
    x <- substr(x, 4, 13)
  } else if (length(grep("NA-[0-9]{4}-NA$", x))>0) {
    # 1736-49
    x <- substr(x, 4, 8)
  } else if (length(grep("[0-9]{4}-[0-9]{1,2}$", x))>0) {
    spl <- unlist(strsplit(x, "-"), use.names = FALSE)
    x <- paste(spl[[1]], paste(substr(spl[[1]], 1, 4-nchar(spl[[2]])), spl[[2]], sep = ""), sep = "-")
  } else if (length(grep("[0-9]{4}/[0-9]*$", x))>0) {
    x <- substr(x, 1, 4)
  } else if (length(grep("[0-9]{4} [0-9]*$", x))>0) {
    # 1780 1800 > 1780-1800
    x <- gsub(" ", "-", x)
  }

  # "( ) 25 1643"
  spl <- unlist(strsplit(x, " "), use.names = FALSE)
  spl <- unique(spl[grep("[0-9]{4}", spl)])
  if (length(spl) == 1) {x <- spl}

  # A couple of quick exits for the impatient  
  # 1690, 1690 -> 1690
  if (length(grep("[0-9]{4}, [0-9]{4}$", x))>0) {  
    x <- as.character(min(as.numeric(unique(unlist(strsplit(x, ","), use.names = FALSE)))))
  }

  if (length(grep("^[0-9]{4}$", x)) > 0) {
    # 1900
    start <- gsub("^([0-9]+)$", "\\1", x)
    end <- NA
    return (c(from=as.numeric(start), till=end))
  } else if (length(grep("^[0-9]{4}[-][0-9]{4}$", x)) > 0) {
    # 1900-1910
    start <- gsub("^([0-9]{4}).*", "\\1", x)
    end <- gsub(".*([0-9]{4})$", "\\1", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))
  }

  # Proceed to more complex cases
  start <- harmonize_names(x, start_synonyms, check.synonymes = F, include.lowercase = F)
  start <- as.character(start)

  if (length(grep("-", x))>0) {
    spl <- unlist(strsplit(as.character(start), "-"), use.names = FALSE)
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
  }

  if (length(grep("^NA[-][0-9]{4}$", x)) > 0) {
    # NA-1910
    start <- NA
    end <- gsub("NA-", "", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))    
  } else if (length(grep("^[0-9]{4}[-]NA$", x)) > 0) {
    # 1900-NA
    start <- gsub("-NA", "", x)
    end <- NA
    return (c(from=as.numeric(start), till=as.numeric(end)))        
  #} else if (length(grep("\\[between [0-9]* [0-9]*\\?\\]", x))>0 || length(grep("\\[between [0-9]* [0-9]*\\]", x))>0 || length(grep("between [0-9]* [0-9]*", x))>0) {
  #  # [between 1790 1800?]  
  #  spl <- unlist(strsplit(x, " "), use.names = FALSE)
  #  start <- spl[[2]]
  #  end <- gsub("\\?$", "", gsub("\\]$", "", spl[[3]]))
  #} else if (length(grep("^\\[[0-9]* or [0-9]*\\]", x)) > 0) {
  #  # [1 or 2]  
  #  start <- unlist(strsplit(gsub("\\[", "", x), "or"), use.names = FALSE)[[1]]
  #  end <- NA    
  } else if (length(grep("\\[[0-9]*\\]", x)) > 0) {
    # MDCCLXVIII. [1768]  
    spl <- unlist(strsplit(x, " "), use.names = FALSE)
    if (length(spl) > 1) {spl <- spl[[2]]} else {spl <- spl[[1]]}
    start <- gsub("\\[", "", gsub("\\]", "", spl))
    end <- NA    
  } else if (length(grep("^[0-9]*\\.", x)) > 0) {
    # "1798. (price one dollar)"  
    start <- unlist(strsplit(x, "\\."), use.names = FALSE)[[1]]
    end <- NA
  #} else if (length(grep("*\\[[0-9]*\\]", x)) > 0) {
  #  # MDCCLXVIII[1768]  
  #  spl <- gsub("\\]", "", unlist(strsplit(x, "\\["), use.names = FALSE)[[2]])
  #  start <- spl
  #  end <- NA    
  } else if (length(grep("[0-9]*", x)) > 0) {
    # MDCCLXVIII. 1768  
    spl <- unlist(strsplit(x, " "), use.names = FALSE)
    if (length(spl) > 1) {spl <- spl[[2]]} else {spl <- spl[[1]]}
    start <- spl
    end <- NA
  }
  
  #start <- start[!start %in% c("", " ")]
  start[start == ""] <- NA
  start[start == " "] <- NA  
  start <- christian2numeric(start) 
  start_year <- as.numeric(start)

  end <- harmonize_names(end, end_synonyms, check.synonymes = F, include.lowercase = F)
  end <- as.character(end)

  end <- christian2numeric(end)   
  end_year <- as.numeric(end)

  if (length(start_year) == 0) {start_year <- NA}
  if (length(end_year) == 0) {end_year <- NA}  
  if (length(start_year) > 1) {start_year <- NA}
  if (length(end_year) > 1) {end_year <- NA}

  c(start_year, end_year)

}



christian2numeric <- function (x) {

  inds <- grep("a.d", x)
  if (length(inds) > 0) {
    x[inds] <- as.numeric(str_trim(gsub("a.d", "", x[inds])))
  }

  inds <- grep("b.c", x)
  if (length(inds) > 0) {
    x[inds] <- -as.numeric(str_trim(gsub("b.c", "", x[inds])))
  }

  x

}

