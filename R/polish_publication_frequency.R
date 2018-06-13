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
  x <- gsub("^ca ", "", x)
  x <- gsub("\\?", " ", x)
  x <- gsub("'", " ", x)    
  x <- gsub(" */ *", "/", x)
  x <- gsub("^[0-9]+ s$", "", x)
  x <- condense_spaces(x)

  f <- system.file("extdata/replace_special_chars.csv", package = "bibliographica")
  spechars <- suppressWarnings(read_mapping(f, sep = ";", mode = "table", include.lowercase = TRUE))
  x <- as.character(map(x, spechars, mode = "match"))

  xorig <- x
  x <- xuniq <- unique(xorig)
  df <- do.call("rbind", lapply(x, polish_publication_frequencies))
  
  # Match to original inds and return
  df[match(xorig, xuniq),]

}


polish_publication_frequencies <- function (x) {

  # Convert with different languages. Use the one with least NAs
  # not an optimal hack but works for the time being..
  tmps <- list()
  tmps[["English"]] <- suppressWarnings(polish_publication_frequency_english(x))
  tmps[["Swedish"]] <- suppressWarnings(polish_publication_frequency_swedish(x))  
  tmps[["Finnish"]] <- suppressWarnings(polish_publication_frequency_finnish(x))
  lang <- names(which.min(sapply(tmps, function (tmp) {sum(is.na(tmp))})))
  tmp <- tmps[[lang]]

  # Convert all units to years
  unityears <- tmp$unit
  unityears <- gsub("year", "1", unityears)  
  unityears <- gsub("month", as.character(1/12), unityears)  
  unityears <- gsub("week", as.character(1/52), unityears)  
  unityears <- gsub("day", as.character(1/365), unityears)  
  unityears <- gsub("Irregular", NA, unityears)
  unityears <- gsub("Single", NA, unityears)    

  suppressWarnings(
    annual <- as.numeric(as.character(tmp$freq)) / as.numeric(unityears)
  )

  # Provide harmonized textual explanations for each frequency
  annual2text <- publication_frequency_text(tmp$unit, annual)

  data.frame(freq = annual2text, annual = annual)

}





#' @title Polish Publication Frequency English
#' @description Harmonize publication frequencies for English data.
#' @param x publication frequency field (a vector) 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_publication_frequency_english("weekly")}
#' @keywords utilities
polish_publication_frequency_english <- function(x) {

  # TODO add to CSV list rather than mixed in the code here				     
  x <- gsub("issued several times a week, frequency of issue varies", "3 per week", x)
  x <- gsub("during the law terms", NA, x)  
  x <- gsub("suday", "sunday", x)
  x <- gsub("colleced", "collected", x)
  x <- gsub(" and index$", "", x)    
  x <- gsub(", when parliament is in session", "", x)
  x <- gsub("\\(when parliament is in session\\)", "", x)
  x <- gsub("\\(when parliament is sitting\\)", "", x)
  x <- gsub("\\(while parliament is sitting\\)", "", x)  
  x <- gsub("\\(published according to sitting dates of parliament\\)", "", x)
  x <- gsub("\\(during the sitting dates of parliament\\)", "", x)
  x <- gsub("\\(during the sitting of parliament\\)", "", x)      
  x <- gsub(", with irregular special issues", "", x)  
  x <- gsub(" \\(collected [a-z]+\\)", "", x)
  x <- gsub(" \\(during the racing season\\)", "", x)
  x <- gsub(" \\(collected issues for [0-9]+\\)", "", x)
  x <- gsub(" \\(collected annually, [0-9]+-[0-9]+\\)", "", x)  
  x <- gsub(" \\(collected [0-9]+ times a [a-z]+\\)", "", x)
  x <- gsub(" \\(collected [a-z]+ [a-z]+\\)", "", x)
  x <- gsub(" \\(collected [a-z]+ [0-9]+ issues\\)", "", x)
  x <- gsub(" \\(compiled issues\\)", "", x)
  x <- gsub(" \\(compilation\\)", "", x)      
  x <- gsub(" \\(collected [a-z]+\\)", "", x)    
  x <- gsub(" \\(cumulati*ve\\)", "", x)
  x <- gsub(" \\(cumulated\\)", "", x)
  x <- gsub(" \\(with general title page\\)", "", x)  
  x <- gsub(" \\(cumulates monthly issues\\)", "", x)
  x <- gsub(" \\(cumulated every [a-z]+ numbers\\)", "", x)  
  x <- gsub(" \\(with [a-z]+ cumulation\\)", "", x)
  x <- gsub(", with annual or semiannual cumulation", "", x)
  x <- gsub(", with annual cumulation and indexes", "", x)  
  x <- gsub(", with [0-9]+-* *[a-z]+ cumulations*", "", x)
  x <- gsub(", with [a-z]+ cumulations*", "", x)  
  x <- gsub(", with cumulation in [0-9]+ volumes*", "", x)  
  x <- gsub(", with [a-z]+ [a-z]+ cumulations*", "", x)  
  x <- gsub(" \\(with [a-z]+ cumulations*\\)", "", x)  
  x <- gsub(" \\(cumulation\\)", "", x)
  x <- gsub(" \\(on mondays\\)", "", x)
  x <- gsub(" \\(on tuesdays\\)", "", x)    
  x <- gsub(" \\(on wednesdays\\)", "", x)
  x <- gsub(" \\(on thursdays\\)", "", x)
  x <- gsub(" \\(on fridays\\)", "", x)
  x <- gsub(" \\(on saturdays\\)", "", x)          
  x <- gsub(" \\(on sundays\\)", "", x)
  x <- gsub(" \\(mondays\\)", "", x)
  x <- gsub(" \\(tuesdays\\)", "", x)
  x <- gsub(" \\(tues\\)", "", x)      
  x <- gsub(" \\(wednesdays\\)", "", x)
  x <- gsub(" \\(thursdays\\)", "", x)
  x <- gsub(" \\(thurs\\.*\\)", "", x)  
  x <- gsub(" \\(fridays\\)", "", x)
  x <- gsub(" \\(saturdays\\)", "", x)          
  x <- gsub(" \\(sundays\\)", "", x)
  x <- gsub(" \\(w?th some variation\\)", "", x)
  x <- gsub(", with occasional supplements", "", x)
  x <- gsub("bi-", "bi", x)
  x <- gsub("semi-", "semi", x)
  x <- gsub("annually", "annual", x)
  x <- gsub(" \\[[0-9]+\\]", "", x)
  x <- condense_spaces(x)
  
  freq <- rep(NA, length = length(x))
  unit <- rep(NA, length = length(x))  

  # English
  f <- system.file("extdata/numbers_english.csv", package = "bibliographica")
  #f <- "~/Rpackages/bibliographica/inst/extdata/numbers_english.csv"
  char2num <- read_mapping(f, sep = ",", mode = "table", from = "character", to = "numeric")
  x <- map(x, synonymes = char2num, from = "character", to = "numeric", mode = "match")

  # every ten issues/numbers
  inds <- c(grep("^every [0-9]+ issues", x), grep("^every [0-9]+ numbers", x))
  if (length(inds)>0) {
    x[inds] <- NA
  }

  # 18 issues per year
  inds <- grep("^[0-9]+ issues ", x)
  if (length(inds)>0) {
    x[inds] <- gsub(" issues", "", x[inds])
  }

  # 3 times weekly/daily/yearly/monthly -> 3 per week
  inds <- grep("^[0-9]+ times [a-z]+ly$", x)
  if (length(inds)>0) {
    x[inds] <- gsub("ly$", "", x[inds])
    x[inds] <- gsub(" times ", " per ", x[inds])
    x[inds] <- gsub(" dai$", " day", x[inds])    
  }

  # daily, except sunday -> 6/week
  inds <- grep("^daily,* except sunday$", x)
  if (length(inds)>0) {
    freq[inds] <- 6
    unit[inds] <- "week"
    x[inds] <- NA # handled    
  }

  # daily (except weekends)
  inds <- grep("^daily,* \\(*except [a-z]+\\. & [a-z]+\\.\\)*$", x)
  if (length(inds)>0) {
    freq[inds] <- 5
    unit[inds] <- "week"
    x[inds] <- NA # handled    
  }

  # daily except sunday
  inds <- grep("^daily,* \\(*except sundays*\\)*$", x)
  if (length(inds)>0) {
    freq[inds] <- 6
    unit[inds] <- "week"
    x[inds] <- NA # handled    
  }

  # daily (except sun.)
  inds <- grep("^daily \\(except [a-z]+\\.\\)$", x)
  if (length(inds)>0) {
    freq[inds] <- 6
    unit[inds] <- "week"
    x[inds] <- NA # handled    
  }

  # daily except weekends
  inds <- grep("^daily,* \\(*except weekends\\)*$", x)
  if (length(inds)>0) {
    freq[inds] <- 5
    unit[inds] <- "week"
    x[inds] <- NA # handled    
  }

  # daily
  inds <- grep("^daily", x)
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "day"
    x[inds] <- NA # handled
  }

  # 2 daily
  inds <- grep("^[0-9]+ daily$", x)
  if (length(inds)>0) {
    freq[inds] <- unlist(strsplit(x[inds], " "), use.names = F)[[1]]
    unit[inds] <- "day"
    x[inds] <- NA # handled    
  }

  # every other day
  inds <- grep("^every other day$", x)
  if (length(inds)>0) {
    freq[inds] <- 1/2
    unit[inds] <- "day"
    x[inds] <- NA # handled    
  }

  # weekly
  inds <- grep("^weekly", x)
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "week"
    x[inds] <- NA # handled    
  }

  # semiweekly
  inds <- grep("^semiweekly$", x)
  if (length(inds)>0) {
    freq[inds] <- 2
    unit[inds] <- "week"
    x[inds] <- NA # handled    
  }

  # semiweekly (tuesday and friday) 
  inds <- grep("^semiweekly \\([a-z]+ and [a-z]+\\)$", x)
  if (length(inds)>0) {
    freq[inds] <- 2
    unit[inds] <- "week"
    x[inds] <- NA # handled    
  }

  # biweekly
  inds <- grep("^biweekly", x)
  if (length(inds)>0) {
    freq[inds] <- 2
    unit[inds] <- "week"
    x[inds] <- NA # handled    
  }

  # triweekly
  inds <- grep("^triweekly$", x)
  if (length(inds)>0) {
    freq[inds] <- 3
    unit[inds] <- "week"
    x[inds] <- NA # handled    
  }

  # monthly
  inds <- grep("^monthly", x)
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "month"
    x[inds] <- NA # handled    
  }

  # bimonthly
  inds <- grep("^bimonthly", x)
  if (length(inds)>0) {
    freq[inds] <- 1/2
    unit[inds] <- "month"
    x[inds] <- NA # handled    
  }

  # semimonthly
  inds <- grep("^semimonthly", x)
  if (length(inds)>0) {
    freq[inds] <- 1/2
    unit[inds] <- "month"
    x[inds] <- NA # handled    
  }

  # quarterly
  inds <- grep("^quarterly", x)
  if (length(inds)>0) {
    freq[inds] <- 4
    unit[inds] <- "year"
    x[inds] <- NA # handled    
  }

  # annual
  inds <- grep("^annual *\\(*", x)
  if (length(inds)>0) {
    freq[inds] <- 1
    unit[inds] <- "year"
    x[inds] <- NA # handled    
  }

  # semiannual
  inds <- grep("^semiannual", x)
  if (length(inds)>0) {
    freq[inds] <- 2
    unit[inds] <- "year"
    x[inds] <- NA # handled    
  }

  # biennial
  inds <- grep("^biennial$", x)
  if (length(inds)>0) {
    freq[inds] <- 1/2
    unit[inds] <- "year"
    x[inds] <- NA # handled    
  }

  # two or three times a year
  inds <- grep("^[0-9]+ or [0-9]+ times a [a-z]+$", x)
  if (length(inds)>0) {
    spl <- strsplit(x[inds], " ")
    freq[inds] <- mean(as.numeric(sapply(spl, function (xi) {xi[[1]]})), as.numeric(sapply(spl, function (xi) {xi[[3]]})))
    unit[inds] <- sapply(spl, function (xi) {xi[[6]]})
    x[inds] <- NA # handled    
  }

  # two or three times a year
  inds <- grep("^[0-9]+ to [0-9]+ times a [a-z]+$", x)
  if (length(inds)>0) {
    spl <- strsplit(x[inds], " ")
    freq[inds] <- mean(as.numeric(sapply(spl, function (xi) {xi[[1]]})), as.numeric(sapply(spl, function (xi) {xi[[3]]})))
    unit[inds] <- sapply(spl, function (xi) {xi[[6]]})
    x[inds] <- NA # handled    
  }

  # three times a year
  inds <- grep("^[0-9]+ times a [a-z]+", x)
  if (length(inds)>0) {
    x[inds] <- gsub(" times a ", " ", x[inds])
    spl <- strsplit(x[inds], " ")
    freq[inds] <- as.numeric(sapply(spl, function (xi) {xi[[1]]}))
    unit[inds] <- sapply(spl, function (xi) {xi[[2]]})
    x[inds] <- NA # handled    
  }

  # three times weekly
  inds <- grep("^[0-9]+ times [a-z]+$", x)
  if (length(inds)>0) {
    spl <- strsplit(x[inds], " ")
    freq[inds] <- as.numeric(sapply(spl, function (xi) {xi[[1]]}))
    unit[inds] <- sapply(spl, function (xi) {xi[[3]]})
    x[inds] <- NA # handled    
  }

  # three per year
  inds <- grep("^[0-9]+ per [a-z]+$", x)
  if (length(inds)>0) {
    spl <- strsplit(x[inds], " ")
    freq[inds] <- as.numeric(sapply(spl, function (xi) {xi[[1]]}))
    unit[inds] <- sapply(spl, function (xi) {xi[[3]]})
    x[inds] <- NA # handled    
  }

  # three times per year
  inds <- grep("^[0-9]+ times per [a-z]+$", x)
  if (length(inds)>0) {
    x[inds] <- gsub(" times per ", " ", x[inds])
    spl <- strsplit(x[inds], " ")
    freq[inds] <- as.numeric(sapply(spl, function (xi) {xi[[1]]}))
    unit[inds] <- sapply(spl, function (xi) {xi[[2]]})
    x[inds] <- NA # handled    
  }

  # 4 issues in 6 months
  inds <- grep("^[0-9]+ issues in [0-9]+ [a-z]+$", x)
  if (length(inds)>0) {
    spl <- strsplit(x[inds], " ")
    n1 <- as.numeric(sapply(spl, function (xi) {xi[[1]]}))
    n2 <- as.numeric(sapply(spl, function (xi) {xi[[4]]}))
    u <- sapply(spl, function (xi) {xi[[5]]})
    freq[inds] <- n1/n2
    unit[inds] <- u
    x[inds] <- NA # handled    
  }

  # 4 in 6 months
  inds <- grep("^[0-9]+ in [0-9]+ [a-z]+$", x)
  if (length(inds)>0) {
    spl <- strsplit(x[inds], " ")
    n1 <- as.numeric(sapply(spl, function (xi) {xi[[1]]}))
    n2 <- as.numeric(sapply(spl, function (xi) {xi[[3]]}))
    u <- sapply(spl, function (xi) {xi[[4]]})
    freq[inds] <- n1/n2
    unit[inds] <- u
    x[inds] <- NA # handled    
  }

  # 1 during a 6teen-month period
  inds <- grep("^[0-9]+ during a [0-9]+-[a-z]+ period", x)
  if (length(inds)>0) {
    x[inds] <- gsub("-", " ", x[inds])
    spl <- strsplit(x[inds], " ")
    n1 <- as.numeric(sapply(spl, function (xi) {xi[[1]]}))
    n2 <- as.numeric(sapply(spl, function (xi) {xi[[4]]}))
    u <- sapply(spl, function (xi) {xi[[5]]})
    freq[inds] <- n1/n2
    unit[inds] <- u
    x[inds] <- NA # handled    
  }

  # every five days
  inds <- grep("^every [0-9]+ [a-z]+$", x)
  if (length(inds)>0) {
    x[inds] <- gsub("^every ", "", x[inds])
    spl <- strsplit(x[inds], " ")
    freq[inds] <- 1/as.numeric(sapply(spl, function (xi) {xi[[1]]}))
    unit[inds] <- sapply(spl, function (xi) {xi[[2]]})
    x[inds] <- NA # handled    
  }

  # twice every three weeks
  inds <- grep("^[0-9]+ every [0-9]+ [a-z]+$", x)
  if (length(inds)>0) {
    spl <- strsplit(x[inds], " ")

    freq[inds] <- as.numeric(sapply(spl, function (xi) {xi[[1]]}))/as.numeric(sapply(spl, function (xi) {xi[[3]]}))
    unit[inds] <- sapply(spl, function (xi) {xi[[4]]})
    x[inds] <- NA # handled    
  }

  # semiannual
  inds <- grep("^semiannual$", x)
  if (length(inds)>0) {
    freq[inds] <- 2
    unit[inds] <- "year"
    x[inds] <- NA # handled    
  }

  # irregular
  inds <- c(
    which(x == "irregular"),
    which(x == "unknown"),
    which(x == "frequency unknown"),        
    which(x == "frequency irregular"),
    which(x == "no determinable frequency")
  )
  if (length(inds)>0) {
    freq[inds] <- NA
    unit[inds] <- "Irregular"
    x[inds] <- NA # handled        
  }

  x <- condense_spaces(x)
  if (is.null(x) || is.na(x) || x == "") {
    # skip
  }

  # Translate units (weeks -> week; years -> year etc)
  unit <- gsub("s$", "", unit)

  # orig = x, 
  data.frame(unit = unit, freq = as.numeric(as.character(freq)))

}



#' @title Polish Publication Frequency Swedish
#' @description Harmonize publication frequencies for Swedish data.
#' @param x publication frequency field (a vector) 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_publication_frequency_swedish("1 nr/ar")}
#' @keywords utilities
polish_publication_frequency_swedish <- function(x) {

  x <- gsub(" h\\.*/", " nr/", x)
  x <- gsub(" hft\\.*/", " nr/", x)
  x <- gsub(" pl\\.*/", " nr/", x)
  x <- gsub(" s. l.nge kryssningarna varar", " ", x)
  x <- condense_spaces(x)
  
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

  # 6-8 nr/termin
  inds <- grep("^[0-9]+-*[0-9]* nr/termin", x)
  if (length(inds) > 0) {
    s <- condense_spaces(gsub("/", "", gsub("[[:lower:]]", "", x[inds])))
    # Termin is 1/2 years, hence multiplying the frequency by 2 to get annual estimate
    freq[inds] <- 2 * sapply(strsplit(s, "-"), function (xi) {mean(as.numeric(xi))})
    unit[inds] <- "year"
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

  # 1 nr/varannan manad	
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

  # Vartannat eller vart trejde ar
  # Vartannat till vart tredje ar 
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

  # irregular
  inds <- grep("^irregular$", x)
  if (length(inds)>0) {
    freq[inds] <- NA
    unit[inds] <- "Irregular"
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

  # TODO add these to separate table
  # Special cases
  inds <- which(x == "vart 3 ar-1 nr/ar") # Every second year
  if (length(inds) > 0) {
    freq[inds] <- 1/2
    unit[inds] <- "year"
  }

  if (is.null(x) || is.na(x)) {
    # skip
  } else if (x == "vart 3 till vart 4 ar") {
    freq <- 1/3.5    
    unit <- "year"
  } else if (x == "1 nr/vecka med sommaruppehall, dvs ca 42 nr/ar") {
    freq <- 42
    unit <- "year"
  } else if (x == "1 nr/vecka (april-sept.), 2 nr/m.nad (okt.-mars)") {
    freq <- 26 + 12 
    unit <- "year"
  } else if (x == "1 nr/vecka (april-sept.)") {
    freq <- 26
    unit <- "year"
  } else if (x == "2 nr/m.nad (1 och 4 kvartalet), 1 nr/kvartal (2 och 3 kvartalet)") {
    freq <- 12 + 2
    unit <- "year"
  } else if (x == "11 nr/ar + arsvol") {
    freq <- 12
    unit <- "year"
  } else if (x == "tidigare 1 nr vart 3 ar, nu 1 nr/ar")  {
    freq <- NA
    unit <- "Irregular"
  } else if (x == "2 nr/m.nad ([0-9]+-[0-9]+). 1 nr/m.nad ([0-9]+-[0-9+]?)")  {
    freq <- NA
    unit <- "Irregular"
  } else if (x == "var 3 dag s. l.nge kryssningarna varar")  {
    freq <- 1/3
    unit <- "day"
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





