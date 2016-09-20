#' @title Polish Publication Frequency
#' @description Harmonize publication frequencies.
#' @param x publication frequency field (a vector) 
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_publication_frequency("Kerran vuodessa")}
#' @keywords utilities
polish_publication_frequency <- function(x) {

  # Remove periods  
  x <- condense_spaces(tolower(gsub("\\.$", "", x)))
  x <- gsub(" /", "/", x)
  f <- system.file("extdata/replace_special_chars.csv",
                package = "bibliographica")
  spechars <- suppressWarnings(read_mapping(f, sep = ";", mode = "table", include.lowercase = TRUE))
  x <- as.character(map(x, spechars, mode = "match"))
  
  xorig <- x
  x <- xuniq <- unique(xorig)
  df <- do.call("rbind", lapply(x, polish_publication_frequencies))
  
  # Match to original inds and return
  df[match(xorig, xuniq),]

}


polish_publication_frequencies <- function(x) {

  # Convert with different languages. Use the one with least NAs
  # not an optimal hack but works for the time being..
  tmps <- list()
  tmps[["Finnish"]] <- suppressWarnings(polish_publication_frequency_finnish(x))
  tmps[["Swedish"]] <- suppressWarnings(polish_publication_frequency_swedish(x))
  tmp <- tmps[[names(which.min(colSums(sapply(tmps, function (tmp) {is.na(tmp)}))))]]

  # Convert all units to years
  unityears <- tmp$unit
  unityears <- gsub("year", "1", unityears)  
  unityears <- gsub("month", as.character(1/12), unityears)  
  unityears <- gsub("week", as.character(1/52), unityears)  
  unityears <- gsub("day", as.character(1/365), unityears)  
  unityears <- gsub("Irregular", NA, unityears)
  unityears <- gsub("Single", NA, unityears)    

  suppressWarnings(
    annual <- tmp$freq / as.numeric(unityears)
  )

  # Provide harmonized textual explanations for each frequency
  annual2text <- publication_frequency_text(tmp$unit, annual)

  data.frame(freq = annual2text, annual = annual)

}





#' @title Polish Publication Frequency Swedish
#' @description Harmonize publication frequencies for Swedish data.
#' @param x publication frequency field (a vector) 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_publication_frequency_swedish("1 nr/ar")}
#' @keywords utilities
polish_publication_frequency_swedish <- function(x) {

  x <- gsub("^ca ", "", x)
  x <- gsub(" h\\.*/", " nr/", x)
  x <- gsub(" hft\\.*/", " nr/", x)
  x <- gsub(" pl\\.*/", " nr/", x)
  
  freq <- rep(NA, length = length(x))
  unit <- rep(NA, length = length(x))  

  # Swedish
  f <- system.file("extdata/numbers_swedish.csv", package = "bibliographica")
  char2num <- read_mapping(f, sep = ",", mode = "table", from = "character", to = "numeric")
  x <- map(x, synonymes = char2num, from = "character", to = "numeric", mode = "match")
  
  # 6 nr/ar / 6-8 nr/ar
  inds <- grep("^[0-9]+-*[0-9]* nr/ar$", x)
  if (length(inds) > 0) {
    s <- condense_spaces(gsub("/", "", gsub("[[:lower:]]", "", x[inds])))
    freq[inds] <- sapply(strsplit(s, "-"), function (xi) {mean(as.numeric(xi))})
    unit[inds] <- "year"
  }

  # 6-8 nr/manad OR 6-8 nr/manaden OR 6-8 nr/man
  inds <- c(
          grep("^[0-9]+-*[0-9]* nr/man *[:lower:| ]*", x),
          grep("^[0-9]+-*[0-9]* nr/manad *[:lower:| ]*", x),	  
          grep("^[0-9]+-*[0-9]* nr/manaden *[:lower:| ]*", x))
  if (length(inds) > 0) {
    s <- gsub("[[:lower:]]", "", x[inds])
    s <- condense_spaces(gsub("/", "", s))
    freq[inds] <- sapply(strsplit(s, "-"), function (xi) {mean(as.numeric(xi))})
    unit[inds] <- "month"
  }

  # 6-8 nr/vecka
  inds <- grep("^[0-9]+-*[0-9]* nr/vecka", x)
  if (length(inds) > 0) {
    s <- condense_spaces(gsub("/", "", gsub("[[:lower:]]", "", x[inds])))
    freq[inds] <- sapply(strsplit(s, "-"), function (xi) {mean(as.numeric(xi))})
    unit[inds] <- "week"
  }

  # 6-8 nr/kvartal
  inds <- grep("^[0-9]+-*[0-9]* nr/kvartal", x)
  if (length(inds) > 0) {
    s <- condense_spaces(gsub("/", "", gsub("[[:lower:]]", "", x)))
    freq[inds] <- 4 * sapply(strsplit(s, "-"), function (xi) {mean(as.numeric(xi))}) # kvartal is 1/4 year
    unit[inds] <- "year"
  }

  # daglig
  inds <- grep("^daglig$", x)
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "day"
  }

  # arligen
  inds <- c(grep("^.rlig$", x), grep("^.rligen$", x))
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "year"
  }

  # 1 nr/varannan månad	
  inds <- c(grep("^[0-9]+ nr/vartannat [[:lower:]]+$", x), grep("^[0-9]+ nr/varannan [[:lower:]]+$", x))
  if (length(inds)>0) {
    spl <- strsplit(x[inds], " ")
    freq[inds] <- as.numeric(sapply(spl, function (xi) {xi[[1]]}))/2
    unit[inds] <- sapply(strsplit(x[inds], " "), function (xi) {xi[[length(xi)]]})
  }
  
  # varannan/vartannat ar/manad/vecka
  inds <- c(grep("^vartannat [[:lower:]]+$", x), grep("^varannan [[:lower:]]+$", x))
  if (length(inds)>0) {
    freq[inds] <- .5
    unit[inds] <- sapply(strsplit(x[inds], " "), function (xi) {xi[[2]]})
  }


  # Vartannat eller vart trejde år
  # Vartannat till vart tredje år 
  inds <- c(grep("^vartannat [[:lower:]]+ vart 3 [[:lower:]]+$", x),
            grep("^varannan [[:lower:]]+ vart 3 [[:lower:]]+$", x)
       )
  if (length(inds)>0) {
    freq[inds] <- 2.5
    unit[inds] <- sapply(strsplit(x[inds], " "), function (xi) {xi[[length(xi)]]})
  }


  # vart x ar
  inds <- grep("^vart [0-9]+ [[:lower:]]+$", x)
  if (length(inds)>0) {
    x[inds] <- gsub("^vart ", "", x[inds])
    freq[inds] <- 1/as.numeric(sapply(strsplit(x[inds], " "), function (xi) {xi[[1]]}))
    unit[inds] <- sapply(strsplit(x[inds], " "), function (xi) {xi[[2]]})
  }

  # Varje vecka
  inds <- grep("^varje [[:lower:]]+$", x)
  if (length(inds)>0) {
    x[inds] <- gsub("^varje ", "", x[inds])
    freq[inds] <- 1
    unit[inds] <- x[inds]
  }

  # Misc
  inds <- unique(c(
       	    grep("^oregelbunden$", x),
       	    grep("^sporadisk$", x)
	  ))
  if (length(inds) > 0) {
    freq[inds] <- NA
    unit[inds] <- "Irregular"
  }
  
  # Translate units in English
  unit <- gsub("^ar$", "year", unit)
  unit <- gsub("manaden", "month", unit)
  unit <- gsub("manad", "month", unit)  
  unit <- gsub("man", "month", unit)  
  unit <- gsub("veckor", "week", unit)
  unit <- gsub("vecka", "week", unit)  
  unit <- gsub("dagar", "day", unit)
  unit <- gsub("dag", "day", unit)    

  data.frame(unit = unit, freq = freq)

}







#' @title Polish Publication Frequency Finnish
#' @description Harmonize publication frequencies for Finnish data.
#' @param x publication frequency field (a vector) 1
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_publication_frequency_finnish("Kerran vuodessa")}
#' @keywords utilities
polish_publication_frequency_finnish <- function(x) {

  freq <- rep(NA, length = length(x))
  unit <- rep(NA, length = length(x))  

  # Finnish
  f <- system.file("extdata/numbers_finnish.csv", package = "bibliographica")
  char2num <- read_mapping(f, sep = ",", mode = "table", from = "character", to = "numeric")
  x <- map(x, synonymes = char2num, from = "character", to = "numeric", mode = "match")

  # yksi-kaksi kertaa/numeroa vuodessa
  inds <- unique(c(
            grep("^[[:lower:]&-]+ kerta+ [[:lower:]]+$", x),
            grep("^[0-9]+-[0-9]+ kerta+ [[:lower:]]+$", x)))
  if (length(inds)>0) {
    x[inds] <- condense_spaces(gsub("kerta+", "", x[inds]))
  }

  # yhdesta kahteen kertaa vuodessa
  inds <- c(grep("^[[:lower:]]+ [[:lower:]]+ kertaa [[:lower:]]+$", x),
            grep("^[0-9]+ *[0-9]* kertaa [[:lower:]]+$", x))
  if (length(inds)>0) {
    x[inds] <- condense_spaces(gsub("kerta+", "", x[inds]))
    n <- sapply(strsplit(x[inds], " "), function (x) {x[-length(x)]})
    freq[inds] <- mean(na.omit(as.numeric(n)))
    unit[inds] <- sapply(strsplit(x[inds], " "), function (xi) {xi[[length(xi)]]})
  }

  # kaksi kertaa vuodessa
  inds <- unique(c(
            grep("^[[:lower:]]+ kerta+ [[:lower:]]+$", x),
            grep("^[0-9]+ kerta+ [[:lower:]]+$", x)	    
	    ))
  if (length(inds)>0) {
    x[inds] <- condense_spaces(gsub("kerta+", "", x[inds]))
  }

  # yksi-kaksi numeroa
  # setting to NA as the interval not given
  # TODO: can be combined with interval or years to calculate frequency
  inds <- unique(c(
            grep("^[[:lower:]&-]+ numero[a]*$", x),
            grep("^[0-9]+-*[0-9]* numero[a]*$", x)))
  if (length(inds)>0) {
    x[inds] <- NA
    freq[inds] <- NA
    unit[inds] <- "Irregular"
  }

  # yksi-kaksi numeroa/numeroa vuodessa
  inds <- unique(c(
            grep("^[[:lower:]&-]+ numero[a]* [[:lower:]]+$", x),
            grep("^[0-9]+-[0-9]+ numero[a]* [[:lower:]]+$", x)))
  if (length(inds)>0) {
    x[inds] <- condense_spaces(gsub("numero[a]*", "", x[inds]))
  }

  # kaksi numeroa/numeroa vuodessa
  inds <- unique(c(
            grep("^[[:lower:]]+ numero[a]* [[:lower:]]+$", x),
            grep("^[0-9]+ numero[a]* [[:lower:]]+$", x)	    
	    ))
  if (length(inds)>0) {
    x[inds] <- condense_spaces(gsub("numero[a]*", "", x[inds]))
  }

  # yksi-kaksi vuodessa
  inds <- unique(c(
            grep("^[[:lower:]&-]+ [[:lower:]]+$", x),
            grep("^[0-9]+-[0-9]+ [[:lower:]]+$", x)))

  if (length(inds)>0) {
    n <- sapply(strsplit(x[inds], " "), function (x) {x[[1]]})
    n <- as.character(n)
    freq[inds] <- sapply(strsplit(n, "-"), function (x) {mean(as.numeric(x))})
    unit[inds] <- sapply(strsplit(x[inds], " "), function (x) {x[[2]]})
  }

  # kaksi vuodessa
  inds <- unique(c(
            grep("^[[:lower:]]+ [[:lower:]]+$", x),
            grep("^[0-9]+ [[:lower:]]+$", x)	    
	    ))
  if (length(inds)>0) {
    n <- sapply(strsplit(x[inds], " "), function (x) {x[[1]]})
    n <- as.character(n)
    freq[inds] <- as.numeric(n)
    unit[inds] <- sapply(strsplit(x[inds], " "), function (x) {x[[2]]})
  }

  # kerran kuussa
  inds <- grep("^kerran [[:lower:]]+$", x)
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- gsub("kerran ", "", x[inds])
  }

  # paivittain
  inds <- grep("^p.ivitt.in$", x)
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "day"
  }

  # kuukausittain
  inds <- grep("^kuukausittain$", x)
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "month"
  }

  # viikottain
  inds <- grep("^viiko[i]*ttain$", x)
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "week"
  }

  # vuosittain
  inds <- grep("^vuosittain$", x)
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "year"
  }

  # joka x vuosi
  inds <- grep("^joka [0-9]+ [[:lower:]]+$", x)  
  if (length(inds)>0) {
    x <- gsub("^joka ", "", x)
    n <- sapply(strsplit(x[inds], " "), function (x) {x[[1]]})
    n <- as.character(n)
    freq[inds] <- 1/as.numeric(n)
    unit[inds] <- sapply(strsplit(x[inds], " "), function (x) {x[[2]]})
  }

  # kerran x vuodessa
  inds <- grep("^kerran [0-9]+ [[:lower:]]+$", x)
  if (length(inds)>0) {
    x <- gsub("^kerran ", "", x)
    n <- sapply(strsplit(x[inds], " "), function (x) {x[[1]]})
    freq[inds] <- 1/as.numeric(n)
    unit[inds] <- sapply(strsplit(x[inds], " "), function (x) {x[[2]]}) 
  }

  # Single publication
  inds <- unique(c(
       	    grep("^ilmestynyt vain", x),
       	    grep("^ilmestynyt kerran$", x),
       	    grep("^ilmestynyt 1$", x),	    
       	    grep("^kertajulkaisu$", x)	    	    	    
	  ))
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "Single"
    #x[inds] <- "Single"        
  }

  # Misc
  inds <- unique(c(
       	    grep("^ep.s..nn.llinen$", x),
       	    grep("^ep.s..nn.llisesti$", x),	    
       	    grep("^ilmestymistiheys vaihtelee$", x),
       	    grep("^vaihtelee$", x),
       	    grep("^vaihdellut$", x),
	    # This could be combined with interval to
	    # calculated frequency
       	    grep("^[[:lower:]]+ numeroa$", x)	    
	  ))
  if (length(inds) > 0) {
    freq[inds] <- NA
    unit[inds] <- "Irregular"
    #x[inds] <- "Irregular"    
  }
  
  # Translate units in English
  unit <- gsub("vuodessa", "year", unit)
  unit <- gsub("vuosi", "year", unit)
  unit <- gsub("kuukaudessa", "month", unit)
  unit <- gsub("kuussa", "month", unit)  
  unit <- gsub("kuukausi", "month", unit)
  unit <- gsub("viikossa", "week", unit)
  unit <- gsub("viikko", "week", unit)
  unit <- gsub("paivittain", "day", unit)  
  unit <- gsub("paivassa", "day", unit)
  unit <- gsub("paiva", "day", unit)

  data.frame(unit = unit, freq = freq)

}





