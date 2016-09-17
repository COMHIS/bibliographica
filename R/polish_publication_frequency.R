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
       	    grep("^ilmestymistiheys vaihtelee$", x),
       	    grep("^vaihtelee$", x),
       	    grep("^vaihdellut$", x),
       	    grep("^seitsem.an numeroa$", x)	    
	  ))
  if (length(inds)>0) {
    freq[inds] <- NA
    unit[inds] <- NA
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

  # Convert all units to years
  unityears <- unit
  unityears <- gsub("year", "1", unityears)  
  unityears <- gsub("month", as.character(1/12), unityears)  
  unityears <- gsub("week", as.character(1/52), unityears)  
  unityears <- gsub("day", as.character(1/365), unityears)

  suppressWarnings(annual <- freq / as.numeric(unityears))

  # Provide harmonized textual explanations for each frequency
  text <- x
  peryear <- as.numeric(annual)
  inds <- is.numeric(peryear) & !is.na(peryear) 
  text[inds] <- peryear[inds]

  text[round(peryear) == 365] <- "Daily"
  text[round(peryear) == 104] <- "Twice per Week"  
  text[round(peryear) == 156] <- "Three per Week"
  text[round(peryear) == 208] <- "Four per Week"  
  text[round(peryear) == 312] <- "Six per Week"    
  text[round(peryear) == 52] <- "Weekly"
  text[round(peryear) == 36] <- "Three per Month"
  text[round(peryear) %in% c(17,18)] <- "Every three Weeks"
  text[round(peryear) == 24] <- "Twice per Month"
  text[round(peryear) == 26] <- "Every two Weeks"  
  text[round(peryear) == 12] <- "Monthly"
  text[round(peryear) == 1] <- "Annual"
  text[round(peryear) == 2] <- "Every six Months"
  text[round(peryear) == 3] <- "Every four Months"
  text[round(peryear) == 4] <- "Every three Months"
  text[round(peryear) == 5] <- "Five per Year"
  text[round(peryear) == 7] <- "Seven per Year"  
  text[round(peryear) == 8] <- "Eight per Year"
  text[round(peryear) == 9] <- "Nine per Year"    
  text[round(peryear) == 6] <- "Every two months"  
  text[round(peryear,1) == .5] <- "Every two Years"
  text[round(peryear,1) == .3] <- "Every three Years"
  text[round(peryear,1) == .25] <- "Every four Years"
  annual[text == "Irregular"] <- NA
  annual[text == "Single"] <- NA  

  # Order the levels by frequency
  text <- factor(text, levels = unique(text[order(unityears)]))
  
  data.frame(freq = text, annual = annual)

}
