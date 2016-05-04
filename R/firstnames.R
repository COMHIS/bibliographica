#' @title First names
#' @description First name list
#' @param languages Vector of languages whose first name will be used.
#' @return Vector of first names
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- firstnames()}
#' @keywords utilities
firstnames <- function (languages = NULL) {

  if (is.null(languages)) {
    languages <- c("French", "German", "English", "Finnish", "Custom", "Pseudonyme")
  }		

  first <- list()

  for (lang in languages) {
  
    if ("French" %in% languages) {
      first[[lang]] <- firstnames_french()
    }

    if ("German" %in% languages) {
      first[[lang]] <- firstnames_german()
    }

    if ("English" %in% languages) {
      first[[lang]] <- firstnames_english()
    }

    if ("Finnish" %in% languages) {
      first[[lang]] <- firstnames_finnish()
    }

    if ("Custom" %in% languages) {
      first[[lang]] <- firstnames_english()
    }

    if ("Pseudonyme" %in% languages) {
      first[[lang]] <- firstnames_pseudo()
    }
    
  }

  # Combine the tables
  first <- rbind_all(first)
  first <- unique(first)

  first$gender <- factor(first$gender)
  
  first

}



#' @title Finnish First Names
#' @description Finnish first name table, including gender info.
#' @param ... Arguments to be passed
#' @return Table with first name and gender info.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- firstnames_finnish()}
#' @details Finnish first names (including gender info) from Vaestorekisterikeskus VRK (see sorvi::get_gender_fi) for details.
#' @import babynames
#' @seealso sorvi::get_gender_fi
#' @keywords utilities
firstnames_finnish <- function (...) {

  first <- get_gender_fi()[, c("name", "gender")]
  first$dictionary <- "Finnish"
    
  first

}



#' @title English First Names
#' @description English first name table, including gender info.
#' @param ... Arguments to be passed
#' @return Table with first name and gender info.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- firstnames_english()}
#' @details English first names (including gender info): For each year from 1880 to 2013, the number of children of each sex given each name. All names with more than 5 uses are given. Source: \url{http://www.ssa.gov/oact/babynames/limits.html}. These are available via the \pkg{babynames} R package.
#' @import babynames
#' @seealso The babynames data is also available in R babynames package. Can combine with that later.
#' @keywords utilities
firstnames_english <- function (...) {

  # English first names (including gender info): For each year from
  # 1880 to 2013, the number of children of each sex given each
  # name. All names with more than 5 uses are given. (Source:
  # \url{http://www.ssa.gov/oact/babynames/limits.html}). Here we use
  # the implementation from the babynames R package.
  first <- unique(babynames[, c("name", "sex")])
  colnames(first) <- c("name", "gender")
  first$dictionary <- "English"

  # Genderizer API queries are limited to 1000 names/day
  # so not useful here on the fly. Can be tested after all other names have been genderized.
  # devtools::install_github("kalimu/genderizeR")
  #library(genderizeR)
  # givenNames = findGivenNames(x, progress = FALSE)  
  #g <- genderize(x, genderDB = givenNames, progress = FALSE)
  #g
    
  first

}






#' @title French first names
#' @description French first names table, including gender info.
#' @param ... Arguments to be passed
#' @return Table with first name and gender info.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- firstnames_french()}
#' @details The French first name lists are collected from the multilingual \url{http://www.lexique.org/public/prenoms.php} (Prenoms.txt) and French \url{http://www.excel-downloads.com/forum/86934-liste-des-prenoms.htmlhttp://http://www.excel-downloads.com/forum/86934-liste-des-prenoms.html}.
#' @keywords utilities
firstnames_french <- function (...) {

  # French lists (multilingual)
  # http://www.lexique.org/public/prenoms.php
  first <- read.csv(system.file("extdata/names/firstnames/French/Prenoms.txt", package = "bibliographica"), header = TRUE, sep = "\t")[, 1:3]
  names(first) <- c("name", "gender", "dictionary")
  first$gender <- gsub("^m,f$", "A", first$gender)
  first$gender <- gsub("^f,m$", "A", first$gender)
  first$gender[first$gender == ""] <- "A"
  first$gender <- gsub("^$", "A", first$gender)
  first$gender <- gsub("^m$", "M", first$gender)
  first$gender <- gsub("^f$", "F", first$gender)

  # French2 
  #http://www.excel-downloads.com/forum/86934-liste-des-prenoms.htmlhttp://http://www.excel-downloads.com/forum/86934-liste-des-prenoms.html
  first2 <- read.csv(system.file("extdata/names/firstnames/French/Prenoms.csv", package = "bibliographica"), header = FALSE)
  first2	<- data.frame(list(name = tolower(first2[,1])))
  first2$gender <- NA
  first2$dictionary <- "French"

  # Combine the lists
  first <- data.frame(rbind(first, first2))
  
  first

}





#' @title German first names
#' @description German first name table, including gender info.
#' @param ... Arguments to be passed
#' @return Table with first name and gender info.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- firstnames_german()}
#' @details The information was collected from \url{http://www.albertmartin.de/vornamen/}.
#' @keywords utilities
firstnames_german <- function (...) {

  # German names
  # http://www.albertmartin.de/vornamen/frauen/
  first.male <- read.csv(system.file("extdata/names/firstnames/German/vornamenMale.csv", package = "bibliographica"), header = FALSE, sep = ";")
  first.female <- read.csv(system.file("extdata/names/firstnames/German/vornamenFemale.csv", package = "bibliographica"), header = FALSE, sep = ";")
  first <- rbind(first.male, first.female)
  first$V2 <- gsub("mannlich", "M", first$V2)
  first$V2 <- gsub("weiblich", "F", first$V2)
  first$V2 <- gsub("unspezifisch", "A", first$V2) # Ambiguous
  names(first) <- c("name", "gender")
  first <- data.frame(first)
  first$dictionary <- "German"

  first

}






#' @title Custom First Names
#' @description Custom first name table, including gender info.
#' @param ... Arguments to be passed
#' @return Table with first name and gender info.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- firstnames_custom()}
#' @keywords utilities
firstnames_custom <- function (...) {

  # Custom first names added manually
  first <- read.csv(system.file("extdata/names/firstnames/custom.csv", package = "bibliographica"), header = FALSE)
  first	<- data.frame(list(name = tolower(first[,1])))
  first$gender <- NA
  first$dictionary <- "Custom_Firstnames"  

  first

}






#' @title Pseudonyme Information
#' @description Pseudonyme first names, including gender info.
#' @param ... Arguments to be passed
#' @return Table with first name (pseudonyme) and gender info.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- firstnames_pseudo()}
#' @keywords utilities
firstnames_pseudo <- function (...) {

  pseudo <- as.character(read.csv(system.file("extdata/names/pseudonymes/custom_pseudonymes.csv", package = "bibliographica"), sep = "\t")[,1])
  pseudo <- data.frame(list(name = unique(tolower(pseudo))))
  pseudo$gender <- NA
  pseudo$dictionary <- "Custom_Pseudonymes"  

  pseudo

}









