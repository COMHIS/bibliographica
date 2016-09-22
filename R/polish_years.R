#' @title Polish Years
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

  x <- gsub("\\.$", "", x)
  x <- gsub(", *\\[*[0-9]\\]*$", "", x)

  if (is.null(start_synonyms)) {
    f <- system.file("extdata/fi_start_years.csv", package = "bibliographica")
    start_synonyms <- read_mapping(f, sep = "\t", mode = "table")
  }
  
  if (is.null(end_synonyms)) {
    f <- system.file("extdata/fi_end_years.csv", package = "bibliographica")
    end_synonyms <- read_mapping(f, sep = "\t", mode = "table")
  }
  
  f <- system.file("extdata/months.csv", package = "bibliographica")
  months <- as.character(read.csv(f, header = TRUE)[,1])
  months <- unique(c(months, tolower(months)))
  # Handle from longest to shortest to avoid problems
  months <- months[rev(order(nchar(months)))]
  
  x0 = x
  xorig <- x <- tolower(as.character(x))
  xuniq <- unique(xorig)
  x <- xuniq
  
  if (verbose) {
    message(paste("Polishing years:", length(xuniq), "unique cases"))
  }

  # "[1.12.1584 jalkeen]" -> 1584 jalkeen
  inds <- grep("[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}", x)
  x[inds] <- gsub("[0-9]{1,2}\\.[0-9]{1,2}\\.", "", x[inds])
  x <- gsub(",", ".", x)

  # 23.1967 -> 1967
  x <- gsub(" [0-9]{1,2}\\.", "", x)   
  x <- gsub("\\.[0-9]$", "", x)

  # "Printed in the Yeare,;1648."
  inds <- grep(";", x)
  x[inds] <- unlist(sapply(x[inds], function (x) {x <- unlist(strsplit(x, ";")); paste(x[grep("[0-9]", x)], collapse = ", ")}), use.names = FALSE)
  x <- gsub("\\.", " ", x)
  
  # 18th century (remove separately before removing other letters)
  x <- gsub("[0-9]{1,4}th", "", x)  
  
  # Remove the remaining letters
  if (length(grep("-+[[:lower:]]*[0-9]{4}-+", x))>0) {
    x <- gsub("[[:lower:]]", "", x)
  }
  
  # Map back to original indices and make unique again. To speedup further.
  xorig <- x[match(xorig, xuniq)]
  x = xuniq <- unique(xorig)
  
  x <- harmonize_ie(x)

  x <- gsub("-a", "- a", x) # -approximately
  x <- remove_print_statements(x)
  
  # Map back to original indices and make unique again. To speedup further.
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  
  x <- sapply(x, function (xi) {handle_ie(xi, harmonize = FALSE)}, USE.NAMES = FALSE)
  x <- condense_spaces(gsub("\\.", " ", x))
  x <- remove_time_info(x, verbose = F, months)

  # Map back to original indices and make unique again. To speedup further.
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  
  # Remove some other info
  x <- gsub("price [0-9] d", "", x)
  x <- gsub("-[0-9]{2,3}\\?{1,2}$", "", x)
  x <- gsub("\\?", "", x)
  x <- gsub("\\!", " ", x)  
  x <- gsub("^& ", "", x)
  x <- condense_spaces(gsub("\\[\\]", " ", x))
  x <- gsub(" -", "-", gsub("- ", "-", x))
  x <- harmonize_christian(x)

  inds <- grep(" or ", x)
  if (length(inds)>0) {
    x[inds] <- sapply(x[inds], function (x) unlist(strsplit(x, " or "), use.names = FALSE)[[2]])
  }
  x[inds] <- str_trim(x[inds])
  
  # Convert romans
  num <- suppressWarnings(as.numeric(as.roman(gsub(" ", "", x))))
  inds <- which(!is.na(num))
  if (length(inds) > 0) {
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

  start_year[is.infinite(start_year)] <- NA
  end_year[is.infinite(end_year)] <- NA  

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
    start <- x # gsub("^([0-9]+)$", "\\1", x)
    end <- NA
    return (c(from=as.numeric(start), till=end))
  } else if (length(grep("^[0-9]+[-|,]+[0-9]+[-|,]+[0-9]+[-|,][0-9]+", gsub(" ", "", x))) > 0) {
    # "1921-1922;1921-1922;1922"
    x <- condense_spaces(gsub(",", " ", gsub("-", " ", x)))
    x <- unlist(strsplit(x, " "), use.names = FALSE)
    x <- na.omit(as.numeric(x))
    start <- min(x)
    end <- max(x)

  } else if (length(grep("^[0-9]+[-|,]+[0-9]+[-|,]+[0-9]", gsub(" ", "", x))) > 0) {
    # "1921-1922;1921
    x <- condense_spaces(gsub(",", " ", gsub("-", " ", x)))
    x <- unlist(strsplit(x, " "), use.names = FALSE)
    x <- na.omit(as.numeric(x))
    start <- min(x)
    end <- max(x)
    
  } else if (length(grep("^[0-9]{4}$", gsub("\\]", "", gsub("\\[", "", x)))) > 0) {
    # 190[1]] / 19[83]
    x <- gsub("\\[", "", x)
    x <- gsub("\\]", "", x)    
    start <- x
    end <- NA
    return (c(from=as.numeric(start), till=end))    
  } else if (length(grep("^\\[*[0-9]{4}\\]*[-]\\[*[0-9]{4}\\]*$", x)) > 0) {
    # 1900-1910 / [1929]-[1930]
    start <- gsub("^\\[*([0-9]{4}).*\\]*", "\\1", x)
    end <- gsub("\\[*.*([0-9]{4})\\]*$", "\\1", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))
  } else if (length(grep("^\\[*[0-9]{4}\\]*[-]\\]* *\\[*[0-9]{4}\\]*$", x)) > 0) {
    # "[1904-] 1905"
    start <- gsub("^\\[*([0-9]{4}).*", "\\1", x)
    end <- gsub(".*([0-9]{4})$", "\\1", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))
  } else if (length(grep("^[0-9]{4}-[0-9]{4}", gsub("\\]", "", gsub("\\[", "", x)))) > 0) {
    # "18[35-18]42"
    x <- gsub("\\]", "", gsub("\\[", "", x))
    spl <- unlist(strsplit(x, "-"), use.names = FALSE)
    start <- spl[[1]]
    end <- spl[[2]]
    return (c(from=as.numeric(start), till=as.numeric(end)))
  } else if (length(grep("^[0-9]{1,4}B\\.C-[0-9]{1,4}B\\.C$", x)) > 0) {
    # 30bc-26bc
    tmp <- as.numeric(gsub("B\\.C", "", unlist(strsplit(x, "-"))))
    start <- -tmp[[1]]
    end <- -tmp[[2]]
    return (c(from=as.numeric(start), till=as.numeric(end)))
  } else if (length(grep("^[0-9]{1,4}B\\.C-[0-9]{1,4}$", x)) > 0) {
    # 30bc-26
    tmp <- as.numeric(gsub("B\\.C", "", unlist(strsplit(x, "-"))))
    start <- -tmp[[1]]
    end <- tmp[[2]]
    return (c(from=as.numeric(start), till=as.numeric(end)))        
  } else if (length(grep("^[0-9]{1,4}B\\.C$", x)) > 0) {
    start <- -as.numeric(gsub("B\\.C", "", x))
    end <- NA
    return (c(from=as.numeric(start), till=as.numeric(end)))    
  } else if (length(grep("^[0-9| ]+$", x))>0) {
    n <- as.numeric(unlist(strsplit(condense_spaces(x), " ")))
    start <- min(n)
    end <- max(n)
    return (c(from=as.numeric(start), till=end))    
  }

  # More complex cases..
  # "mdccx-mdccxi [1710-1711]" -> [1710-1711]
  if (length(grep("[[:lower:]]*-[[:lower:]]* \\[[0-9]*-[0-9]*\\]", x))>0) {
    x <- remove_letters(x)
  } else if (length(grep("^[0-9]{4} *\\[[0-9]\\]$", x))>0) {
    # "1646[7]" -> 1647
    x <- gsub(" \\[", "\\[", x)
    x <- paste(substr(x, 1, 3), substr(x, 6, 6), sep = "")
  } else if (length(grep("^[0-9]{4}-\\[[0-9]*\\]", x))>0) {
    # "1646-[47]" -> 1646-47
    x <- gsub("\\[", "", gsub("\\]", "", x))
  } else if (length(grep("^[0-9]{4}\\[[0-9]{4}\\]-[0-9]{2}", x))>0) {
    # "1646[1647]-50" -> 1647-50
    x <- gsub("\\]", "", substr(x, 6, nchar(x)))
  } else if (length(grep("^[0-9]{4}, [0-9]\\:o$", x))>0) {
    # "1731, 4:o" -> 1731
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
  x <- gsub("[[:lower:]]", "", x)
  x <- condense_spaces(x)

  if (length(grep("^[0-9]{2}\\[[0-9]{2}\\]$", x))>0) {
    # 19[99]
    x <- gsub("\\[", "", x)
    x <- gsub("\\]", "", x)    
  } else if (length(grep("^[0-9]{3}\\[[0-9]{1}\\]$", x))>0) {
    # 199[9]
    x <- gsub("\\[", "", x)
    x <- gsub("\\]", "", x)    
  } else if (length(grep("^[0-9]{4}/[0-9] \\[[0-9]{4}\\]$", x))>0) {
    # 1695/6 [1696]
    x <- gsub("\\]", "", unlist(strsplit(x, "\\["), use.names = FALSE)[[2]])
  } else if (length(grep("^[0-9]{4} {0,1}\\[[0-9]*\\]$", x))>0) {
    # 1726[1727] -> 1727
    x <- gsub(" \\[", "[", x)
    x <- substr(x, 6, 9)
  } else if (all(!unlist(strsplit(x, "")) %in% letters) && length(grep("\\[[0-9]\\]", x))>0) {
    x <- gsub("\\[", " ", x)
    x <- gsub("\\]", " ", x)
    x <- gsub("\\(", " ", x)
    x <- gsub("\\)", " ", x)    
    x <- gsub("-", " ", x)
    x <- gsub(";", " ", x)
    x <- gsub(",", " ", x)
    x <- gsub("/", " ", x)
    x <- unlist(strsplit(x, " "), use.names = FALSE)
    x <- na.omit(as.numeric(x))
    x <- x[x >= 100] # ignore years below 100
    x <- paste(min(x), max(x), sep = "-")
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
  x <- gsub(" -", "-", gsub("- ", "-", x))  
  x <- gsub("^-", "NA-", gsub("-$", "-NA", x))
  x <- gsub("[[:lower:]]", "", x)
  x <- condense_spaces(x)
  x <- gsub("^[\\:|\\=]", "", x)
  if (length(x) == 1 && (x == "" || is.na(x))) {x <- "NA"}
  if (length(x) > 1) {
    x <- na.omit(x)
  }

  x <- gsub(" *-+ *","-",x)

  if (length(grep("\\([0-9]+ */*[0-9]+\\)-[0-9]+ *\\([0-9]{4}/*[0-9]+", x))>0) {
    # "1(1861/1865)-8(1896/1900"
    x <- gsub("\\(", " ", x)
    x <- gsub("\\)", " ", x)
    x <- gsub("/", " ", x)
    x <- gsub("-", " ", x)            
    x <- unlist(strsplit(x, " "), use.names = FALSE)
    x <- na.omit(as.numeric(x))
    x <- x[x >= 100] # ignore years below 100
    x <- paste(min(x), max(x), sep = "-")  
  } else if (length(grep("^[0-9]{4}/[0-9]{4}-[0-9]{4}/[0-9]{4}$", x))>0) {
    # "1885/1886- 1894/1895"
    x <- na.omit(as.numeric(unlist(strsplit(unlist(strsplit(x, "-"), use.names = FALSE), "/"), use.names = FALSE)))
    x <- paste(min(x), max(x), sep = "-")
  } else if (length(grep("NA-[0-9]{4}-[0-9]{4}$", x))>0) {
    # NA-1524-1700
    x <- substr(x, 4, 13)
  } else if (length(grep("NA-[0-9]{4}-NA$", x))>0) {
    # 1736-49
    x <- substr(x, 4, 8)
  } else if (length(grep("[0-9]{4}-[0-9]{1,2}$", x))>0) {
    spl <- unlist(strsplit(x, "-"), use.names = FALSE)
    x <- paste(spl[[1]], paste(substr(spl[[1]], 1, 4-nchar(spl[[2]])), spl[[2]], sep = ""), sep = "-")
  } else if (length(grep("^[0-9]{4}/[0-9]*$", x))>0) {  
    x <- substr(x, 1, 4)
  } else if (length(grep("[0-9]{4} [0-9]*$", x))>0) {
    # 1780 1800 > 1780-1800
    x <- gsub(" ", "-", x)
  }

  #x <- unlist(strsplit("3: 1-26 ( 1904/1905 ) , 1: : 1-54 (1905/1906", ""))
  #x <- unlist(strsplit(x, ""))  
  #x <- as.numeric(x)
  #x[is.na(x)] <- "-"
  #x <- condense_spaces(paste(x, collapse = ""))
  #x <- unlist(strsplit(x, "-"), use.names = FALSE)
  #x <- na.omit(as.numeric(x))
  #x <- x[x >= 100] # ignore less than 100
  #x <- paste(min(x), max(x), sep = "-")

  # "( ) 25 1643"
  spl <- unlist(strsplit(x, " "), use.names = FALSE)
  spl <- unique(spl[grep("[0-9]{4}", spl)])
  if (length(spl) == 1) {x <- spl}
  
  # 1690, 1690 -> 1690
  if (length(grep("[0-9]{4}, [0-9]{4}$", x))>0) {  
    x <- as.character(min(as.numeric(unique(unlist(strsplit(x, ","), use.names = FALSE)))))
  }

  # "1885/1886--1889/1890-1885-1885-2"
  x <- gsub("'", " ", x)
  x <- gsub(",", " ", x)    
  x <- gsub("\\(", " ", x)
  x <- gsub("\\)", " ", x)
  x <- gsub(" -+", "-", x)
  x <- gsub("-+ ", "-", x)
  x <- gsub(":", " ", x)
  x <- gsub("\\.", " ", x)
  x <- condense_spaces(x)
  x <- gsub(" ", "-", x)
  x <- gsub("-+", "-", x)

  if (length(grep("^[0-9|/|-]+$", x))>0) {
    n <- unlist(strsplit(unlist(strsplit(x, "-"), use.names = F), "/"), use.names = F)
    n <- unlist(strsplit(unlist(strsplit(n, "\\("), use.names = F), "\\)"), use.names = F)
    n <- na.omit(as.numeric(n))
    n <- n[n>=500] # do not accept years below this one
    start <- NA
    if (length(n) > 0) {start <- min(n)}
    end <- NA
    if (length(n)>1) {end <- max(n)}
    return (c(from=as.numeric(start), till=as.numeric(end)))  
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
  start <- map(x, start_synonyms)
  start <- as.character(start)

  if (length(grep("-", x))>0) {
    spl <- unlist(strsplit(as.character(start), "-+"), use.names = FALSE)
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
  } else if (length(grep("[0-9]+", x)) > 0) {
    # MDCCLXVIII. 1768
    spl <- unlist(strsplit(unlist(strsplit(x, " "), use.names = FALSE), "-"), use.names = FALSE)
    spl <- na.omit(as.numeric(spl))
    spl <- spl[spl > 100] # Do not accept years earlier than this
    #if (length(spl) > 1) {spl <- spl[[2]]} else {spl <- spl[[1]]}
    #start <- spl
    #end <- NA
    start <- min(spl)
    end <- NA
    if (length(spl) > 1) {end <- max(spl)}
  } 
  
  start[start == ""] <- NA
  start[start == " "] <- NA  
  start <- christian2numeric(start) 
  start_year <- as.numeric(start)
  
  end <- map(end, end_synonyms)
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

