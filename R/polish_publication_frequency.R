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

  f <- system.file("extdata/replace_special_chars.csv",
                package = "bibliographica")
  spechars <- suppressWarnings(read_mapping(f, sep = ";", mode = "table", include.lowercase = TRUE))
  x <- as.character(map(x, spechars, mode = "match"))

  # Recognize language
  if (length(c(grep("vuosi", x), grep("numero", x)))>0) {
    tmp <- polish_publication_frequency_finnish(x)
  } else if (length(c(grep("manad", x), grep("vecka", x)))>0) {
    tmp <- polish_publication_frequency_swedish(x)  
  }

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
  annual2text <- publication_frequency_text(x, annual)

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

  freq <- rep(NA, length = length(x))
  unit <- rep(NA, length = length(x))  

  # Swedish
  f <- system.file("extdata/numbers_swedish.csv", package = "bibliographica")
  char2num <- read_mapping(f, sep = ",", mode = "table", from = "character", to = "numeric")
  
  # 6 nr/ar / 6-8 nr/ar
  inds <- grep("^[0-9]+-*[0-9]* nr/ar$", x)
  if (length(inds) > 0) {
    s <- condense_spaces(gsub("/", "", gsub("[[:lower:]]", "", x)))
    s <- unlist(strsplit(s, "-"), use.names = FALSE)
    freq[inds] <- mean(as.numeric(s))
    unit[inds] <- "year"
  }

  # 6-8 nr/manad
  inds <- grep("^[0-9]+-*[0-9]* nr/manad$", x)
  if (length(inds) > 0) {
    s <- condense_spaces(gsub("/", "", gsub("[[:lower:]]", "", x)))
    s <- unlist(strsplit(s, "-"), use.names = FALSE)
    freq[inds] <- mean(as.numeric(s))
    unit[inds] <- "month"
  }

  # 6-8 nr/vecka
  inds <- grep("^[0-9]+-*[0-9]* nr/vecka", x)
  if (length(inds) > 0) {
    s <- condense_spaces(gsub("/", "", gsub("[[:lower:]]", "", x)))
    s <- unlist(strsplit(s, "-"), use.names = FALSE)
    freq[inds] <- mean(as.numeric(s))
    unit[inds] <- "week"
  }

  # 6-8 nr/kvartal
  inds <- grep("^[0-9]+-*[0-9]* nr/kvartal", x)
  if (length(inds) > 0) {
    s <- condense_spaces(gsub("/", "", gsub("[[:lower:]]", "", x)))
    s <- unlist(strsplit(s, "-"), use.names = FALSE)
    freq[inds] <- 4 * mean(as.numeric(s))
    unit[inds] <- "year"
  }

  # daglig
  inds <- grep("^daglig$", x)
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "day"
  }

  # arligen
  inds <- grep("^arligen$", x)
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "year"
  }

  # vartannat ar
  inds <- grep("^vartannat ar$", x)
  if (length(inds)>0) {
    freq[inds] <- .5
    unit[inds] <- "year"
  }

  # Misc
  inds <- unique(c(
       	    grep("^oregelbunden$", x)
	  ))
  if (length(inds) > 0) {
    freq[inds] <- NA
    unit[inds] <- "Irregular"
    x[inds] <- "Irregular"    
  }
  
  # Translate units in English
  unit <- gsub("ar", "year", unit)
  unit <- gsub("manad", "month", unit)
  unit <- gsub("vecka", "week", unit)
  unit <- gsub("dag", "day", unit)  

  data.frame(unit = unit, freq = freq)

}







#' @title Polish Publication Frequency Finnish
#' @description Harmonize publication frequencies for Finnish data.
#' @param x publication frequency field (a vector) 
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

  # yksi-kaksi kertaa/numeroa vuodessa
  inds <- unique(c(
            grep("^[[:lower:]&-]+ kerta+ [[:lower:]]+$", x),
            grep("^[0-9]+-[0-9]+ kerta+ [[:lower:]]+$", x)))
  if (length(inds)>0) {
    x[inds] <- condense_spaces(gsub("kerta+", "", x[inds]))
  }

  # yhdesta kahteen kertaa vuodessa
  inds <- grep("^[[:lower:]]+ [[:lower:]]+ kertaa [[:lower:]]+$", x)
  if (length(inds)>0) {
    x[inds] <- condense_spaces(gsub("kerta+", "", x[inds]))
    n <- sapply(strsplit(x[inds], " "), function (x) {x[1:2]})
    n <- as.character(map(n, synonymes = char2num, from = "character", to = "numeric", mode = "match"))
    freq[inds] <- mean(as.numeric(n))
    unit[inds] <- sapply(strsplit(x[inds], " "), function (x) {x[[3]]})

  }
  
  # kaksi kertaa vuodessa
  inds <- unique(c(
            grep("^[[:lower:]]+ kerta+ [[:lower:]]+$", x),
            grep("^[0-9]+ kerta+ [[:lower:]]+$", x)	    
	    ))
  if (length(inds)>0) {
    x[inds] <- condense_spaces(gsub("kerta+", "", x[inds]))
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
    n <- as.character(map(n, synonymes = char2num, from = "character", to = "numeric", mode = "match"))
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
    n <- as.character(map(n, synonymes = char2num, from = "character", to = "numeric", mode = "match"))
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
  inds <- grep("^joka [[:lower:]]+ [[:lower:]]+$", x)  
  if (length(inds)>0) {
    x <- gsub("^joka ", "", x)
    n <- sapply(strsplit(x[inds], " "), function (x) {x[[1]]})
    n <- as.character(map(n, synonymes = char2num, from = "character", to = "numeric", mode = "match"))
    freq[inds] <- 1/as.numeric(n)
    unit[inds] <- sapply(strsplit(x[inds], " "), function (x) {x[[2]]})
  }

  # kerran x vuodessa
  inds <- grep("^kerran [[:lower:]]+ [[:lower:]]+$", x)  
  if (length(inds)>0) {
    x <- gsub("^kerran ", "", x)
    n <- sapply(strsplit(x[inds], " "), function (x) {x[[1]]})
    n <- map(n, synonymes = char2num, from = "character", to = "numeric", mode = "match")
    freq[inds] <- 1/as.numeric(n)
    unit[inds] <- sapply(strsplit(x[inds], " "), function (x) {x[[2]]}) 
  }

  # Single publication
  inds <- unique(c(
       	    grep("^ilmestynyt vain kerran$", x),
       	    grep("^ilmestynyt kerran$", x),
       	    grep("^kertajulkaisu$", x)	    	    	    
	  ))
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- NA
    x[inds] <- "Single"        
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
    x[inds] <- "Irregular"    
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





