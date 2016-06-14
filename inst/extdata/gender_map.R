#' @title Gender Table for First Names
#' @description Combines first name - gender mappings from various sources in French, German, English, Finnish, and Custom lists, including pseudonymes.
#' @param dictionaries Vector of language catalogues to include.
#' @return Table with name-gender mappings.
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
#'     \item{genderdata}{Data sets from the genderdata R packages.}
#'   }
#' @examples \dontrun{gendermap <- gender_map()}
#' @keywords utilities
gender_map <- function (dictionaries = NULL) {

  # Name-gender mappings table; pooled from various sources
  firstnames <- NULL
  first <- firstnames(dictionaries = dictionaries)
  first$name <- iconv(first$name, from = "latin1", to = "UTF-8")

  inds <- c(which(first$name %in% c("paul\n1920 1 18671301 97 05 54 47 370 2 536 5360 1 1 97 05 1 1866 1 100 21 2100 21 2100 21 2100 0 1 1 10 tucker")))
  if (length(inds) > 0) {
    first <- first[-inds, ]
  }

  first$name = tolower(first$name)
  first = unique(first)

  first %>% arrange(name)

  first

}




#' @title Gender Table for First Names
#' @description Combines first name - gender mappings from various sources in French, German, English, Finnish, and Custom lists
#' @param dictionaries Vector of language catalogues to include.
#' @return Vector of first names
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- firstnames()}
#' @keywords utilities
firstnames <- function (dictionaries = NULL) {

  require(gender)
  require(genderdata)

  name <- NULL

  if (is.null(dictionaries)) {
    dictionaries <- c("Multilingual", "French", "German", "English", "Finnish", "Custom", "ssa_state", "ssa_national", "napp", "kantrowitz", "ipums")
  }		

  first <- list()

  for (lang in dictionaries) {

    message(lang)

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
    } else if (lang == "ssa_state") {
      f <- firstnames_genderdata_ssa_state()
    } else if (lang == "ssa_national") {
      f <- firstnames_genderdata_ssa_national()    
    } else if (lang == "kantrowitz") {
      f <- firstnames_genderdata_kantrowitz()    
    } else if (lang == "ipums") {
      f <- firstnames_genderdata_ipums()    
    } else if (lang == "napp") {
      f <- firstnames_genderdata_napp()    
    }

    first[[lang]] <- f
    
  }

  message("Combine the tables")
  first <- rbind_all(first)
  
  message("Harmonize")
  first$name <- condense_spaces(gsub("\\.", " ", first$name))
  first$name <- condense_spaces(gsub("\\,", " ", first$name))
  first$name <- condense_spaces(gsub("\\?", " ", first$name))
  first$name <- condense_spaces(gsub("^-+", "", first$name))
  first$name <- condense_spaces(gsub("^[0-9]+$", "", first$name))  
  first <- first[nchar(first$name) > 1,]
  first$gender <- harmonize_gender(first$gender)
  first$dictionary <- tolower(first$dictionary)  

  # Mark ambiguous genders from the combined tables
  fu <- unique(first[, c("name", "gender")])
  duplicated.names <- unique(fu$name[duplicated(fu$name)])  
  fs <- subset(fu, name %in% duplicated.names)
  spl <- split(fs$gender, fs$name)
  amb <- names(which(sapply(spl, function (x) {length(unique(na.omit(x)))}) > 1))
  first[first$name %in% amb, "gender"] <- "ambiguous"

  # Remove duplicates, entries from some dictionaries may be lost
  # but that should not matter for analysis
  first <- first[!duplicated(first[, c("name", "gender")]),]

  # Now remove cases with NA gender
  # This way we keep all available gender info and
  # get rid of duplicate names like faride -> male / NA
  # which retrieves a unique mapping faride -> male
  first <- dplyr::filter(first, !is.na(gender)) %>% select(name, gender)

  # Clean up and return
  unique(first)

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

  require(fennica)		   
  first <- get_gender_fi()[, c("name", "gender")]
  first$dictionary <- "finnish"
  # Clean up
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

  #babynames <- NULL
  require(babynames)
  # English first names (including gender info): For each year from
  # 1880 to 2013, the number of children of each sex given each
  # name. All names with more than 5 uses are given. (Source:
  # \url{http://www.ssa.gov/oact/babynames/limits.html}). Here we use
  # the implementation from the babynames R package.
  data("babynames")

  first <- unique(babynames[, c("name", "sex")])

  colnames(first) <- c("name", "gender")
  first$dictionary <- "english"

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









firstnames_genderdata_napp <- function (...) {

  # Mappings from the genderdata R package
  # Convert into a table with name, gender, dictionary
  # Can contain duplicates as the years are removed

  data("napp")
  tab <- napp
  tab$gender <- c("female", "male")[apply(tab[, c("female", "male")], 1, function (x) {which.max(x)})]
  
  tab <- tab[, c("name", "gender", "country")]
  tab <- rename(tab, dictionary = country)

  tab <- unique(tab)

  tab

}




firstnames_genderdata_ipums <- function (...) {

  # Mappings from the genderdata R package
  # Convert into a table with name, gender, dictionary
  # Can contain duplicates as the years are removed

  data("ipums_usa")
  tab <- ipums_usa
  tab$gender <- c("female", "male")[apply(tab[, c("female", "male")], 1, function (x) {which.max(x)})]
  
  tab <- tab[, c("name", "gender")]
  tab$dictionary <- "usa"
  tab <- unique(tab)
  tab$name <- gsub("\"", "", tab$name)

  tab

}


firstnames_genderdata_kantrowitz <- function (...) {

  # Mappings from the genderdata R package
  # Convert into a table with name, gender, dictionary
  # Can contain duplicates as the years are removed

  data("kantrowitz")
  tab <- kantrowitz
  
  tab <- tab[, c("name", "gender")]
  
  tab$dictionary <- "kantrowitz"
  tab <- unique(tab)

  tab

}




firstnames_genderdata_ssa_national <- function (...) {

  # Mappings from the genderdata R package
  # Convert into a table with name, gender, dictionary
  # Can contain duplicates as the years are removed

  data("ssa_national")
  tab <- ssa_national

  tab$gender <- c("female", "male")[apply(tab[, c("female", "male")], 1, function (x) {which.max(x)})]

  tab <- tab[, c("name", "gender")]

  tab$dictionary <- "usa"

  tab <- unique(tab)

  tab

}



firstnames_genderdata_ssa_state <- function (...) {

  # Mappings from the genderdata R package
  # Convert into a table with name, gender, dictionary
  # Can contain duplicates as the years are removed

  data("ssa_state")
  tab <- ssa_state

  tab$gender <- c("F", "M")[apply(tab[, c("F", "M")], 1, function (x) {which.max(x)})]

  tab <- tab[, c("name", "gender")]

  tab$dictionary <- "usa"

  tab <- unique(tab)

  tab

}











