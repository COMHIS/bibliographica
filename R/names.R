#' @title firstnames
#' @description First name list
#'
#' @param ... Arguments to be passed
#' @return Vector of first names
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x <- firstnames()}
#' @keywords utilities
firstnames <- function (...) {

  # Validate using publicly available name databases of English names:
  # First names (also gender info): http://www.ssa.gov/oact/babynames/names.zip USA Social Security Administration website; names 1880-2013. In 2013 file we have 33072 names and gender info. Source: http://www.ssa.gov/oact/babynames/limits.html
  first.english <- read.csv(system.file("extdata/names/firstnames/English/yob2013.txt", package = "bibliographica"), header = FALSE)[, 1:2]
  names(first.english) <- c("name", "gender")
  first.english <- data.frame(first.english)
  first.english$dictionary <- "English"

  # Same for German names
  # http://www.albertmartin.de/vornamen/frauen/
  first.german.male <- read.csv(system.file("extdata/names/firstnames/German/vornamenMale.csv", package = "bibliographica"), header = FALSE, sep = ";")
  first.german.female <- read.csv(system.file("extdata/names/firstnames/German/vornamenFemale.csv", package = "bibliographica"), header = FALSE, sep = ";")
  first.german <- rbind(first.german.male, first.german.female)
  first.german$V2 <- gsub("mannlich", "M", first.german$V2)
  first.german$V2 <- gsub("weiblich", "F", first.german$V2)
  first.german$V2 <- gsub("unspezifisch", "A", first.german$V2) # Ambiguous
  names(first.german) <- c("name", "gender")
  first.german <- data.frame(first.german)
  first.german$dictionary <- "German"

  # French lists (multilingual)
  # http://www.lexique.org/public/prenoms.php
  first.french <- read.csv(system.file("extdata/names/firstnames/French/Prenoms.txt", package = "bibliographica"), header = TRUE, sep = "\t")[, 1:3]
  names(first.french) <- c("name", "gender", "dictionary")
  first.french$gender <- gsub("^m,f$", "A", first.french$gender)
  first.french$gender <- gsub("^f,m$", "A", first.french$gender)
  first.french$gender[first.french$gender == ""] <- "A"
  first.french$gender <- gsub("^$", "A", first.french$gender)
  first.french$gender <- gsub("^m$", "M", first.french$gender)
  first.french$gender <- gsub("^f$", "F", first.french$gender)

  # French2 
  #http://www.excel-downloads.com/forum/86934-liste-des-prenoms.htmlhttp://​http://www.excel-downloads.com/forum/86934-liste-des-prenoms.html
  first.french2 <- read.csv(system.file("extdata/names/firstnames/French/Prenoms.csv", package = "bibliographica"), header = FALSE)
  first.french2	<- data.frame(list(name = tolower(first.french2[,1])))
  first.french2$gender <- NA
  first.french2$dictionary <- "French"

  # Custom first names added manually
  first.custom <- read.csv(system.file("extdata/names/firstnames/custom.csv", package = "bibliographica"), header = FALSE)
  first.custom	<- data.frame(list(name = tolower(first.custom[,1])))
  first.custom$gender <- NA
  first.custom$dictionary <- "Custom_Firstnames"  

  # Also add custom last names
  # since in many cases the original catalogue has erroneous notation
  # mixing first and last names and we do not want to filter out valid names if they
  # happen to be in wrong field last/first
  #last.custom <- as.character(read.csv(system.file("extdata/names/lastnames/custom.csv", package = "bibliographica"), sep = "\t")[,1])
  #last.custom	<- data.frame(list(name = tolower(last.custom)))
  #last.custom$gender <- NA
  #last.custom$dictionary <- "Custom_Lastnames"  

  # Also accept pseudonymes
  pseudo <- as.character(read.csv(system.file("extdata/names/pseudonymes/first.csv", package = "bibliographica"), sep = "\t")[,1])
  pseudo <- data.frame(list(name = tolower(pseudo)))
  pseudo$gender <- NA
  pseudo$dictionary <- "Custom_Pseudonymes_Firstname"  

  # Combine the lists
  first <- data.frame(rbind(first.english, first.german, first.french, first.french2, first.custom, pseudo))
  
  first$gender <- factor(first$gender)

  first
}

#' @title lastnames
#' @description Vector of last names
#'
#' @param ... Arguments to be passed
#' @return Vector of last names
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x <- lastnames()}
#' @keywords utilities
lastnames <- function (...) {

  # Last names: http://www2.census.gov/topics/genealogy/1990surnames/dist.all.last United States Census Bureau. Source: http://www.census.gov/topics/population/genealogy/data/1990_census/1990_census_namefiles.html (also first names available here but less names than in the baby database)
  last.census <- as.character(read.csv(system.file("extdata/names/lastnames/last.csv", package = "bibliographica"), sep = "\t")[,1])

  # Custom last names
  last.custom <- as.character(read.csv(system.file("extdata/names/lastnames/custom.csv", package = "bibliographica"), sep = "\t")[,1])

  # Also add custom first names
  # since in many cases the original catalogue has erroneous notation
  # mixing first and last names and we do not want to filter out valid names if they
  # happen to be in wrong field last/first
  # first.custom <- as.character(read.csv(system.file("extdata/names/firstnames/custom.csv", package = "bibliographica"), sep = "\t")[,1])

  # Also accept pseudonymes
  pseudo <- as.character(read.csv(system.file("extdata/names/pseudonymes/last.csv", package = "bibliographica"), header = TRUE)[,1])

  last <- unique(c(last.census, last.custom, pseudo))

  last
}


#' @title notnames
#' @description List of strings that are not names (manually checked)
#'
#' @param ... Arguments to be passed
#' @return Vector of last names
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x <- notnames()}
#' @keywords utilities
notnames <- function (...) {

  generic <- as.character(read.csv(system.file("extdata/names/notnames/generic.csv", package = "bibliographica"), sep = "\t")[,1])
  organization <- as.character(read.csv(system.file("extdata/names/notnames/organization.csv", package = "bibliographica"), sep = "\t")[,1])


  union(generic, organization)

}


#' @title validate_names
#' @description Validate names
#'
#' @param namelist Vector of names to be validated
#' @param database Specify the name database to be used 
#' @return List with following elements:
#'    \itemize{
#'      \item validated Logical vector indicating the valid names
#'	\item invalid List of invalid name components
#'    }
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x <- validate_names(namelist, database)}
#' @keywords utilities
validate_names <- function (namelist, database) {

  # Get name lists from public databases
  # TODO: validating names could make its own R package later,
  # perhaps combined with the gender package?
  
  if (database == "last") {
    accepted_names <- tolower(as.character(lastnames()))
    not_names <- tolower(as.character(notnames()))
  } else if (database == "first") {
    accepted_names <- as.character(firstnames()$name)
    # Also accept individual letters for first names
    accepted_names <- tolower(unique(c(accepted_names, letters)))
    not_names <- tolower(as.character(notnames()))
  }

  # Many names have multiple parts
  # Split to components and check each component is among accepted names
  uniq.names <- unique(namelist)
  uniq.names.spl <- strsplit(uniq.names, " ")
  uniq.names.spl2 <- lapply(uniq.names.spl, function (x) {unlist(strsplit(x, "-"))})
  uniq.names.spl3 <- lapply(uniq.names.spl2, function (x) {str_trim(x)})

  # Accept names where all components (Jean-Luc -> Jean Luc for instance) are 
  # among accepted names
  ok <- sapply(uniq.names.spl3, function (x) {all(x %in% accepted_names)})

  validated <- namelist %in% unique(namelist)[ok]

  # Final accepted names
  # accepted <- unique(namelist[validated]) 

  # List name components that cannot be validated from public name databases
  # and not included in the custom stopword list
  sdif <- setdiff(unlist(uniq.names.spl3), c(not_names, accepted_names, "", "NA", NA))
  counts <- rev(sort(table(sdif)))
  invalid <- data.frame(list(Name = names(counts), Count = counts))

  list(validated = validated, invalid = invalid)

}

