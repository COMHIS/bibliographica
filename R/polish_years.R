#' @title Polish Years
#' @description Pick and polish the year interval (start and end years) from a time field which is of the form 1800 or 1823-1845 etc.
#' @param x year field (a vector) 
#' @param start_synonyms Synonyme table for start year
#' @param end_synonyms Synonyme table for end year
#' @param verbose verbose
#' @param min.year Minimum year accepted
#' @param max.year Maximum year accepted
#' @param check If true, remove entries (replace by NA) where start > end
#' @return data.frame with the fields 'start' and 'end'
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_years(c("1746", "1745-1750"))}
#' @keywords utilities
polish_years <- function(x, start_synonyms=NULL, end_synonyms=NULL, verbose = TRUE, check = FALSE, min.year = -3000, max.year = as.numeric(format(Sys.time(), "%Y")) + 50) {

  # Delete suspiciously long strings
  inds <- which(nchar(x) > 200 & grepl("\t", x))
  if (length(inds) > 0) {
    x[inds] <- NA
    warning(paste("Removed ", length(inds), "publication year entries that are suspiciously long (over 200 characters) and include tabs"))
  }

  x <- gsub("\\.$", "", x)
  x <- gsub("\\[blank\\]", "?", x)
  x <- gsub("\\[sic\\.\\]", "", x)    
  x <- gsub("^&lt;", "", x)
  x <- gsub("&gt;$", "", x)
  x <- gsub("&gt;-$", "", x)
  x <- gsub(", *\\[*[0-9]\\]*$", "", x)
  x <- gsub("\\[̂", "[", x)
  #x <- gsub("\\?\\]", "]", x)
  #x <- gsub("\\[[0-9]{3}-\\]", "", x)
  x <- gsub("(^|[-[])[0-9]{3}[?]", "\\1", x)
  x <- gsub("(^|[-[])[0-9]{2}[?][?]", "\\1", x)
  x <- gsub("I([0-9]{3})([^0-9]|$)", "1\\1", x)
    # 1974 [p.o. 1976] -> 1976
  x <- gsub("[0-9]{4} ?[[]p[.]?o[.]? ?([0-9]{4})[]]", "\\1", x)
  x <- gsub("[\\^]", "", x)
  x <- gsub("[\\|]", "", x)
  x <- gsub("[u]", "", x)

  inds <- grep("\\[*[M,C,D,X,L,\\.]*\\]*", x)
  if (length(inds) > 0) {
    x[inds] <- sapply(x[inds], function (x) {gsub("\\.", "", x)})
  }

  inds <- intersect(grep("^--", x), grep("--$", x))
  x[inds] <- gsub("--$", "", gsub("^--", "", x[inds]))

  inds <- grep("^\\[*l[0-9]{3}\\.*\\]*$", x)
  if (length(inds) > 0) {
    x[inds] <- sapply(x[inds], function (x) {gsub("l", "1", x)})
  }

  f <- system.file("extdata/months.csv", package = "bibliographica")
  months <- as.character(read.csv(f, header = TRUE)[,1])
  months <- unique(c(months, tolower(months)))
  # Handle from longest to shortest to avoid problems
  months <- months[rev(order(nchar(months)))]

  # These seem unnecessary assignments.
  xorig <- tolower(as.character(x))
  xuniq <- unique(xorig)
  x <- xuniq
	
  if (verbose) {
    message(paste("Polishing years:", length(xuniq), "unique cases"))
  }

  # "[1.12.1584 jalkeen]" -> 1584 jalkeen
  inds <- grep("[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}", x)
  x[inds] <- gsub("[0-9]{1,2}\\.[0-9]{1,2}\\.", "", x[inds])
  x <- gsub(",", ".", x)

  # 23.1967 -> 1967 excluding 1.5.6.7
  if (length(grep("[0-9]\\.[0-9]\\.[0-9]\\.[0-9]", x)) == 0) {
    x <- gsub(" [0-9]{1,2}\\.", "", x)
    x <- gsub("\\.[0-9]$", "", x)
  } else {
    x <- gsub("\\.", "", x)
  }

  # "Printed in the Yeare,;1648."
  inds <- grep(";", x)
  x[inds] <- unlist(sapply(x[inds], function (x) {x <- unlist(strsplit(x, ";")); paste(x[grep("[0-9]", x)], collapse = ", ")}), use.names = FALSE)
  x <- gsub("\\.", " ", x)
  x <- gsub(" or later", " ", x)  
  x <- gsub("0[0-9]*;m[a-z]*terio [a-z]*\\.* pauli", " ", x)

  # 18th century (remove separately before removing other letters)
  x <- gsub("[0-9]{1,4}th", "", x)  

  # Map back to original indices and make unique again. To speedup further.
  xorig <- x[match(xorig, xuniq)]

  x <- xuniq <- unique(xorig)
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

  # 1642 [1643] -> 1643
  if (length(grep("^[0-9]* \\[[0-9]*\\]$", x)) > 0) {
    inds <- grep("^[0-9]* \\[[0-9]*\\]$", x)
    for (i in inds) {
      spl <- unlist(strsplit(x[[i]], " "))      
      if (length(spl) > 1) {
        x[[i]] <- spl[[2]]
      } else {
        x[[i]] <- spl[[1]]
      }
    }
  }



  # 1642[1643] -> 1643
  if (length(grep("^[0-9]{4}\\[[0-9]{4}\\]$", x)) > 0) {
    inds <- grep("^[0-9]{4}\\[[0-9]{4}\\]$", x)
    for (i in inds) {
      x[[i]] <- substr(x[[i]], 6, 9)
    }
  }

  # 1642[1643]-45 -> 1643-1645
  if (length(grep("^[0-9]{4}\\[[0-9]{4}\\]-[0-9]{2}$", x)) > 0) {
    inds <- grep("^[0-9]{4}\\[[0-9]{4}\\]-[0-9]{2}$", x)
    for (i in inds) {
      spl <- strsplit(x[[i]], "-")[[1]]
      start <- pick_year(spl[[1]])
      end <- paste0(substr(spl[[1]], 1, 2), spl[[2]])
      x[[i]] <- paste(start, end, sep = "-")
    }
  }

  # 1642[3] -> 1643
  inds <- grep("^[0-9]{4}\\[[0-9]{1}\\]$", x)
  if (length(inds) > 0) {
    for (i in inds) {
      x[[i]] <- paste0(substr(x[[i]], 1, 3), substr(x[[i]], 6, 6))
    }  
  }

  clean2 <- function (x) {
  
    x <- strsplit(x, "\\]-\\[")[[1]]

    start <- polish_year(x[[1]])[[1]]

    end <- NA
    if (length(x) > 1) {
      end <- polish_year(x[[2]])
      if (length(end) > 0) {end <- end[[1]]}
    }

    if (!is.na(start) & !is.na(end) & end < start) {
      end <- ""
    }
    
    x <- paste(start, end, "-")          

  } 


  # "[1900?]-[190-?]"
  inds <- grep("\\]-\\[", x)
  if (length(inds) > 0) {
    x[inds] <- sapply(x[inds], function (xx) {clean2(xx)})
  }

  # "[1900?]-190-?"
  clean1 <- function (x) {
    x <- strsplit(x, "\\]-")[[1]]

    start <- polish_year(x[[1]])

    end <- ""
    if (length(x) > 1) {
      end <- polish_year(x[[2]])
    }
    
    x <- paste(start, end, "-")    
  }

  inds <- grep("\\]-[?]*", x)
  if (length(inds) > 0) {
    x[inds] <- sapply(x[inds], function (xx) {clean1(xx)})
  }

  # 1966 [=1971]  -> 1971
  x <- gsub("[0-9]{4}  ?[[][=]([0-9]{4})[]]", "\\1", x)
  
  # Remove some other info
  x <- gsub("price [0-9] d", "", x)
  x <- gsub("-[0-9]{2,3}\\?{1,2}$", "", x)
  x <- gsub("\\?", "", x)
  x <- gsub("\\!", " ", x)  
  x <- gsub("^& ", "", x)

  x <- condense_spaces(gsub("\\[\\]", " ", x))
  x <- gsub(" -", "-", gsub("- ", "-", x))
  x <- gsub("-+", "-", x)
  x <- gsub("1̂", "1", x)

  x <- gsub("\\[[a-z| ]*\\]", "", x)

  x <- gsub("[[]", "", x)
  x <- gsub("[]]", "", x)
  x <- harmonize_christian(x)

  inds <- grep(" or ", x)
  if (length(inds)>0) {
    x[inds] <- sapply(x[inds], function (x) unlist(strsplit(x, " or "), use.names = FALSE)[[2]])
  }
  x[inds] <- str_trim(x[inds])

  # Convert romans
  x <- gsub("m\\.d", "md", x)
  x <- gsub("m\\.d\\.", "md", x)
  x <- gsub("m d", "md", x)
  x <- gsub("m d ", "md", x)  
  x <- gsub("m\\,d", "md", x)
  x <- gsub("m\\,d\\,", "md", x)
  x <- gsub("md x", "mdx", x)
  x <- gsub("md l", "mdl", x)  
  x <- gsub("ij$", "ii", x)

  num <- suppressWarnings(as.numeric(as.roman(gsub(" ", "", x))))
  inds <- which(!is.na(num))
  if (length(inds) > 0) {
    x[inds] <- num[inds]
  }

  # Map back to original indices and make unique again. To speedup further.
  xorig <- x[match(xorig, xuniq)]
  xuniq <- unique(xorig)
  x <- xuniq

  if (length(grep("[0-9]{4}\\[[0-9]", x))>1) {
    x <- substr(x, 1, 4)
  }

  res <- suppressWarnings(
    lapply(x,
           function (xi) {
             a <- try(polish_year(xi,
                                  start_synonyms = start_synonyms,
                                  end_synonyms = end_synonyms,
                                  months,
                                  verbose)
                      )

             if (class(a) == "try-error") {
               return(c(NA, NA))
             } else {
               return(a)
             }
           }
    )
  )

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

  # Do not accept years below and above these values	    
  start_year[start_year < min.year | start_year > max.year] <- NA
  end_year[end_year < min.year | end_year > max.year] <- NA  

  df <- data.frame(from = as.numeric(as.character(start_year)),
                   till = as.numeric(as.character(end_year)))

  # Match the unique cases to the original indices
  # before returning the df
  return(df[match(xorig, xuniq), ])
  
}




#' @title Polish year
#' @description Pick and polish the year interval (start and end years) from a time field '
#' @param x year string
#' @param start_synonyms Synonyme table for start year
#' @param end_synonyms Synonyme table for end year
#' @param months months
#' @param verbose verbose
#' @param min.year Minimum year accepted
#' @param max.year Maximum year accepted
#' @return vector with the fields 'from' and 'till'
#' @author Leo Lahti and Niko Ilomaki \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_year(c("1746"))}
#' @keywords utilities
polish_year <- function(x, start_synonyms = NULL, end_synonyms = NULL, months, verbose = FALSE, min.year = -3000, max.year = 2100) {

  x <- gsub("^\\[", "", x)
  x <- gsub("\\]$", "", x)
  x <- gsub("\\?$", "", x)

  # x <- gsub("[0-9]{1,2} [a-z+]", "", x)
  if (length(grep("^[a-z|=]*-*[a-z|=]+[0-9]*", x)) > 0) {
     x <- gsub("^[a-z|-|=]*-*[a-z|-|=]+", "", x)
     x <- gsub("[a-z|=]+", "", x)
     x <- gsub("- +", "", x)     
  }

  # Some quick returns for simple cases to speed up
  if (length(grep("^[0-9]{4}$", x)) > 0) {
  
    # 1900
    start <- x # gsub("^([0-9]+)$", "\\1", x)
    end <- NA
    return (c(from=as.numeric(start), till=end))
  } else if (length(grep("^[0-9]{3}-$", x)) > 0) {
  
    # 190-
    start <- gsub("-", "0", x)
    end <- NA
    return (c(from=as.numeric(start), till=end))    

  } else if (length(grep("^[0-9]{4}\\?$", x)) > 0) {

    # 1900?
    start <- gsub("\\?", "", x)
    end <- NA
    return (c(from=as.numeric(start), till=end))

  } else if (length(grep("^[0-9]{3}-\\?$", x)) > 0) {

    # 190-?
    start <- gsub("-\\?", "0", x)

    end <- NA
    return (c(from=as.numeric(start), till=end))

  } else if (length(grep("^[0-9]{2}-$", x)) > 0) {

    # 19-
    start <- paste0(substr(x, 1, 2), "00")
    end <- NA
    return (c(from=as.numeric(start), till=end))

  } else if (is.na(x)) {
    return(c(NA, NA))
	} else if (length(grep("^[0-9]{1,3}[.]?$", x)) >  0) {
		return(c(NA, NA))
	} else if (length(grep("^[0-9]{5}[.]?$", x)) >  0) {
		return(c(NA, NA))
  } else if (length(grep("^[0-9]{4}[-]$", x)) > 0) {
  # 1994-
		start <- gsub("^([0-9]{4})-", "\\1", x)
		end <- NA
	return(c(start, end))
  } else if ((x == "-") | (x == "[-]")) {
  # - ; [-]
	return(c(NA, NA))
  } else if (length(grep("^[0-9]+[-|,]+[0-9]+[-|,]+[0-9]+[-|,][0-9]+", gsub(" ", "", x))) > 0) {
    # "1921-1922;1921-1922;1922"
    x <- condense_spaces(gsub(",", " ", gsub("-", " ", x)))
    x <- unlist(strsplit(x, " "), use.names = FALSE)
    x <- na.omit(as.numeric(x))
    start <- min(x)
    end <- max(x)
   } else if (x=="fin") {
   return(c(NA, NA))
  } else if (length(grep("^[0-9]+[-|,]+[0-9]+[-|,]+[0-9]", gsub(" ", "", x))) > 0) {
    # "1921-1922;1921
    x <- condense_spaces(gsub(",", " ", gsub("-", " ", x)))
    x <- unlist(strsplit(x, " "), use.names = FALSE)
    x <- na.omit(as.numeric(x))
    start <- min(x)
    end <- max(x)
  } else if (x=="NA-NA" | x=="NA-" | x=="-NA" | x=="-") {
	return(c(NA, NA))
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
  } else if (length(grep("^(NA)?[-][0-9]{4}$", x)) > 0) {
    # NA-1910; -1910
    start <- NA
    end <- gsub("(NA)?[-](.*)", "\\2", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))    
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
   } else if (length(grep("^[-][0-9]{4}$", x)) > 0) {
    x <- gsub("^-", "NA-", x)
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
  
  if (length(x) == 1 && (x == "" || is.na(x))) {
    x <- "NA"
  }
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
  
  if (length(spl) == 1) {
    x <- spl
  }
  
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
    n <- na.omit(as.numeric(n))
    n <- n[n>=500] # do not accept years below this one in this special case

    # there shouldn't be any of these as they've already been gsubbed to " " above
    # n <- unlist(strsplit(unlist(strsplit(as.character(n), "\\("), use.names = F), "\\)"), use.names = F)
    # n <- na.omit(as.numeric(n))

    start <- NA
    if (length(n) > 0) {
      start <- min(n)
    }
    end <- NA
    if (length(n) > 1) {
      end <- max(n)
    }
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
  if (!is.null(start_synonyms)) {
    start <- map(x, start_synonyms)
	start <- as.character(start)
   } else {
	start <- x
   }

  if (length(grep("-", x))>0) {
    spl <- unlist(strsplit(as.character(start), "-+"), use.names = FALSE)
    spl <- as.numeric(spl)
	
    # HR 20190225: Do not make this assumption. It's harmful.
	#if (sum(is.na(spl))>1) {
    #  # NA, 3, NA -> 3, NA (assume the year is start year if it is not clear)
    #  spl <- spl[min(which(!is.na(c(NA, 3, NA)))):length(spl)]
    #}
    #x <- spl
    start <- spl[[1]]
	#print(spl)
    if (length(spl) > 1) {
      end <- spl[[2]]
    } else {
      end <- NA
    }
    #x <- paste(x, collapse = "-")
  }
	
  if (length(grep("^(NA)?[-][0-9]{4}$", x)) > 0) {
    # NA-1910; -1910
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
    if (length(spl) > 1) {
      spl <- spl[[2]]} else {spl <- spl[[1]]
    }
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
    if (length(spl) > 1) {
      end <- max(spl)
    }
  } else if (is.na(x)) {
    # somewhere along the way some NAs get in, possibly.
    # That produced a stream of error messages. -vv
    end <- NA
  } else if (x == "NA") {
    end <- NA
  }
	
  start[start == ""] <- NA
  start[start == " "] <- NA  

  start <- christian2numeric(start) 
  start_year <- as.numeric(start)

  # FIXME "cannot coerce type 'closure' to vector of type 'character'" error here.
  # There are cases where end has not yet been assigned at this point. Therefore
  # function below catches to a function base R called end() and hilarity ensues.
  # I'll implement a quick fix for now. -vv
  if (!(exists("end", mode = "character") || exists("end", mode = "numeric"))) {
    end <- NA
  }
  
  if (!is.null(end_synonyms)) {
	end <- map(end, end_synonyms)
	end <- as.character(end)
   }
  end <- christian2numeric(end)   
  end_year <- as.numeric(end)
  
  if (length(start_year) == 0) {start_year <- NA}
  if (length(end_year) == 0) {end_year <- NA}  
  if (length(start_year) > 1) {start_year <- NA}
  if (length(end_year) > 1) {end_year <- NA}

  return(c(start_year, end_year))
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
  
  return(x)
}

