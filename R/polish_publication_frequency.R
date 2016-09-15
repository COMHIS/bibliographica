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
  x <- tolower(gsub("\\.$", "", x))

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
    x[inds] <- "kertajulkaisu"        
  }

  # Misc
  inds <- unique(c(
       	    grep("^epasaannollinen$", x),
       	    grep("^ilmestymistiheys vaihtelee$", x),
       	    grep("^vaihtelee$", x),
       	    grep("^vaihdellut$", x)
	  ))
  if (length(inds)>0) {
    freq[inds] <- NA
    unit[inds] <- NA
    x[inds] <- "vaihteleva"    
  }

  # Convert all units to years
  unit <- gsub("vuodessa", "1", unit)
  unit <- gsub("vuosi", "1", unit)
  unit <- gsub("year", "1", unit)  
  unit <- gsub("kuukaudessa", as.character(1/12), unit)
  unit <- gsub("kuussa", as.character(1/12), unit)  
  unit <- gsub("kuukausi", as.character(1/12), unit)
  unit <- gsub("month", as.character(1/12), unit)  
  unit <- gsub("viikossa", as.character(1/52), unit)
  unit <- gsub("viikko", as.character(1/52), unit)
  unit <- gsub("week", as.character(1/52), unit)  
  unit <- gsub("paivittain", as.character(1/365), unit)  
  unit <- gsub("paivassa", as.character(1/365), unit)
  unit <- gsub("paiva", as.character(1/365), unit)
  unit <- gsub("day", as.character(1/365), unit)
  unit <- gsub("single", "kertajulkaisu", unit)
  unit <- gsub("irregular", "vaihteleva", unit)          

  suppressWarnings(annual <- freq / as.numeric(unit))

  data.frame(freq = x, annual = annual)

}
