#' @title Gender Table for First Names
#' @description Combines first name - gender mappings from various sources in French, German, English, Finnish, and Custom lists, including pseudonymes.
#' @param languages Vector of language catalogues to include.
#' @return Vector of first names
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @details Data sources:
#'   \itemize{
#'     \item{French}{Multilingual database \url{http://www.lexique.org/public/prenoms.php} (Prenoms.txt) and French \url{http://www.excel-downloads.com/forum/86934-liste-des-prenoms.htmlhttp://http://www.excel-downloads.com/forum/86934-liste-des-prenoms.html}.}
#'     \item{German}{\url{http://www.albertmartin.de/vornamen/}}
#'     \item{English}{For each year from 1880 to 2013, the number of children of each sex given each name. All names with more than 5 uses are given: \url{http://www.ssa.gov/oact/babynames/limits.html}). Here we use the implementation from the babynames R package. The name-gender mappings from different years are combined.}
#'     \item{Finnish}{Vaestorekisterikeskus VRK (see fennica::get_gender_fi) for details.}
#'     \item{Custom, Pseudonyme}{Provided by the authors of the bibliographica R package.}
#'   }
#' @examples \dontrun{x <- firstnames()}
#' @keywords utilities
firstnames <- function (languages = NULL) {

  name <- NULL

  if (is.null(languages)) {
    languages <- c("Multilingual", "French", "German", "English", "Finnish", "Custom", "Pseudonyme")
  }		

  first <- list()

  for (lang in languages) {

    if (lang == "Multilingual") {
      f <- firstnames_multilingual()
    } else if (lang == "French") {
      f <- firstnames_french()
    } else if (lang == "German") {
      f <- firstnames_german()
    } else if (lang == "English") {
      f <- firstnames_english()
    } else if (lang == "Finnish") {
      f <- firstnames_finnish()
    } else if (lang == "Custom") {
      f <- firstnames_english()
    } else if (lang == "Pseudonyme") {
      f <- firstnames_pseudo()
    }

    first[[lang]] <- f
    
  }

  # Combine the tables
  first <- rbind_all(first)

  # Harmonize
  first$name <- tolower(first$name)
  first$gender <- factor(toupper(first$gender), c("F", "M", "A"))
  first$dictionary <- tolower(first$dictionary)

  # Mark ambiguous genders
  duplicated.names <- unique(f$name[duplicated(f$name)])  
  fs <- subset(first, name %in% duplicated.names)
  spl <- split(fs$gender, fs$name)
  amb <- names(which(sapply(spl, function (x) {length(unique(na.omit(x)))}) > 1))
  first[first$name %in% amb, "gender"] <- "A"
  first <- unique(first)

  # Gender name synonymes
  fn <- system.file("extdata/harmonize_gender.csv", package = "bibliographica")
  sn <- read_synonymes(fn, sep = ";", mode = "table")
  first$gender <- as.character(suppressWarnings(harmonize_names(first$gender, synonymes = sn)))

  # Combine dictionaries (now all genders should match for any given name)
  # Nothing to combine for now
  # duplicated.names <- unique(first$name[duplicated(first$name)])
  # fs <- subset(first, name %in% duplicated.names)
  # spl <- split(first$dictionary, first$name)

  # Clean up
  first <- as_data_frame(unique(first))
  
  first

}



#' @title Finnish First Names
#' @description Finnish first name table, including gender info.
#' @param ... Arguments to be passed
#' @return Table with first name and gender info.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- firstnames_finnish()}
#' @details Finnish first names (including gender info) from Vaestorekisterikeskus VRK (see fennica::get_gender_fi) for details.
#' @seealso fennica::get_gender_fi
#' @keywords utilities
firstnames_finnish <- function (...) {

  first <- get_gender_fi()[, c("name", "gender")]
  first$dictionary <- "finnish"

  # Clean up further
  first$gender <- gsub("female", "F", first$gender)  
  first$gender <- gsub("male", "M", first$gender)
  first$name <- condense_spaces(gsub("\\.", " ", first$name))

  first

}



#' @title English First Names
#' @description English first name table, including gender info.
#' @param ... Arguments to be passed
#' @return Table with first name and gender info.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- firstnames_english()}
#' @details English first names (including gender info): For each year from 1880 to 2013, the number of children of each sex given each name. All names with more than 5 uses are given from the \pkg{babynames} R package.
#' @keywords utilities
firstnames_english <- function (...) {

  babynames <- NULL

  # English first names (including gender info): For each year from
  # 1880 to 2013, the number of children of each sex given each
  # name. All names with more than 5 uses are given. (Source:
  # \url{http://www.ssa.gov/oact/babynames/limits.html}). Here we use
  # the implementation from the babynames R package.
  first <- unique(babynames[, c("name", "sex")])
  colnames(first) <- c("name", "gender")
  first$dictionary <- "english"

  # Genderizer API queries are limited to 1000 names/day
  # so not useful here on the fly. Can be tested after all other names have been genderized.
  # devtools::install_github("kalimu/genderizeR")
  #library(genderizeR)
  # givenNames = findGivenNames(x, progress = FALSE)  
  #g <- genderize(x, genderDB = givenNames, progress = FALSE)
  #g
    
  first

}




#' @title Multilingual first names
#' @description Multilingual first names table, including gender info.
#' @param ... Arguments to be passed
#' @return Table with first name and gender info.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- firstnames_multilingual()}
#' @details Source: multilingual \url{http://www.lexique.org/public/prenoms.php} (Prenoms.txt).
#' @keywords utilities
firstnames_multilingual <- function (...) {

  # http://www.lexique.org/public/prenoms.php
  first <- read.csv(system.file("extdata/names/firstnames/French/Prenoms.txt", package = "bibliographica"), header = TRUE, sep = "\t")[, 1:3]
  names(first) <- c("name", "gender", "dictionary")
  first$gender <- gsub("^m,f$", "A", first$gender)
  first$gender <- gsub("^f,m$", "A", first$gender)
  first$gender[first$gender == ""] <- "A"
  first$gender <- gsub("^$", "A", first$gender)
  first$gender <- gsub("^m$", "M", first$gender)
  first$gender <- gsub("^f$", "F", first$gender)

  # Clean up further
  ## "kari (2)" -> kari
  first$name <- condense_spaces(gsub("\\([0-9]*\\)", "", first$name))
  first$name <- capitalize(first$name)
  	
  first

}


#' @title French first names
#' @description French first names table, including gender info.
#' @param ... Arguments to be passed
#' @return Table with first name and gender info.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- firstnames_french()}
#' @details The French first name lists are collected from \url{http://www.excel-downloads.com/forum/86934-liste-des-prenoms.htmlhttp://http://www.excel-downloads.com/forum/86934-liste-des-prenoms.html}.
#' @keywords utilities
firstnames_french <- function (...) {

  # French2
  #http://www.excel-downloads.com/forum/86934-liste-des-prenoms.htmlhttp://http://www.excel-downloads.com/forum/86934-liste-des-prenoms.html
  first <- read.csv(system.file("extdata/names/firstnames/French/Prenoms.csv", package = "bibliographica"), header = FALSE)
  first	<- data.frame(list(name = tolower(first[,1])))
  first$gender <- NA
  first$dictionary <- "French"

  # Clean up further
  ## "kari (2)" -> kari
  first$name <- condense_spaces(gsub("\\([0-9]*\\)", "", first$name))
  first$name <- capitalize(first$name)
  	

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
  first$V2 <- gsub("m.nnlich", "M", first$V2)
  first$V2 <- gsub("weiblich", "F", first$V2)
  first$V2 <- gsub("unspezifisch", "A", first$V2) # Ambiguous
  names(first) <- c("name", "gender")
  first <- data.frame(first)
  first$dictionary <- "german"

  

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
  first$dictionary <- "custom_firstnames"  

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
  pseudo$dictionary <- "custom_pseudonymes"  

  pseudo

}









