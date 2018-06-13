#' @title Extract Personal Names
#' @description Extracts personal names.
#' @param x Vector of publisher names
#' @param languages A vector of languages which are used in detecting relation keywords
#' @return Data frame: orig, initials, family, full_name, init_name, guessed, relation
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @examples # extract_personal_names(x, languages=c("finnish", "swedish", "latin"))
#' @keywords utilities
extract_personal_names  <- function(x, languages=c("english")) {

  # To avoid warning in pkg build / LL
  f <- NULL

  message("Starting extract_personal_names")
  orig <- x
  # First, some of the words must be lowercased, so they won't be recognized as names
  x <- decapitate_keywords(x, languages=languages)  
  x <- harmonize_abbreviated_names(x, languages=languages)
  
  message("... decapitated")
  # Prepare everything, so that they have equal length
  family_name <- character(length=length(x))
  initials <- character(length=length(x))
  init_name <- character(length=length(x))
  full_name_with_initials <- character(length=length(x))
  relation <- character(length=length(x))
  guessed <- character(length=length(x))
  
  # Try if the form is "Merckell, Johan Cristopher"
  relation <- get_relation_keyword(x, NULL, languages=languages)
  inds <- which(relation == "")
  message("... get_relation_keyword done")
  # HR 2016-10-06: Now accepts abbreviated names like "Fred."
  full_name <- gsub("^([[:upper:]][[:lower:]]+), ((([[:upper:]][[:lower:]]+[.]?|[[:upper:]][.])( |$))+)", "\\2 \\1", x)
  message("... full_name caught")
  
  #full_name <- gsub("^([[:upper:]][[:lower:]]+), ((([[:upper:]][[:lower:]]+|[[:upper:]][.])( |$))+)", "\\2 \\1", x)
  
  # Try if the form is "Merckell, Johan Cristopherin leski"
  inds <- which(relation != "")
  # HR 2016-10-06: Now accepts abbreviated names like "Fred."
  pattern <- paste("(^[[:upper:]][[:lower:]]+), ((([[:upper:]][[:lower:]]+[.]?|[[:upper:]][.])( |))+)(:n)? ", relation, "$", sep="")
  full_name[inds] <- str_replace(x[inds], pattern=pattern[inds], replacement=paste0("\\2 \\1 ", relation[inds]))
  
  # If full_name is still unchanged, it wasn't caught yet: try again the normal way
  inds <- which(full_name==x)
  message("... before by_words")
  
  # First: try with prefixed "by", "af" etc...
  f <- system.file("extdata/by_words.csv", package="bibliographica")
  by_words <- read.csv(f, sep="\t", fileEncoding="UTF-8")
  by_w <- paste0(as.character(by_words$synonyme), collapse = "|" )
  by_w <- paste0(" (", by_w, ") ")
  # HR 2016-10-06: Now accepts abbreviated names like "Fred."
  full_name[inds] <- str_extract(x[inds], paste0(by_w, "((([[:upper:]][[:lower:]]+[.]?) |([[:upper:]][.] ?)))+[[:upper:]][[:lower:]]+"))
  full_name[inds] <- str_extract(full_name[inds], "((([[:upper:]][[:lower:]]+[.]?) |([[:upper:]][.] ?)))+[[:upper:]][[:lower:]]+")
  message("... after by_words")
  
  # Then those without the by_words
  # HR 2016-10-06: Now accepts abbreviated names like "Fred."
  inds <- which(is.na(full_name))
  full_name[inds] <- str_extract(x[inds], "((([[:upper:]][[:lower:]]+[.]?) |([[:upper:]][.] ?)))+[[:upper:]][[:lower:]]+")
  
  message("... before number_of_given_names is decided")
  # Make sure that number of given names is not negative
  # HR 2016-10-06: Now counts abbreviated names like "Fred."
  number_of_given_names <- unname(sapply(full_name, function(name) {
    max((str_count(name, "(([[:upper:]][[:lower:]]+)|([[:upper:]][.]))") - 1),0)
  }))
  given_names_pattern <- paste("((([[:upper:]][[:lower:]]+[.]?) |([[:upper:]][.] ?))){", (number_of_given_names), "}", sep = "")
  
  message("... before given_names is decided")
  given_names <- str_extract(full_name, given_names_pattern)
  initials <- gsub("[[:lower:]]+[.]? ", ".", given_names)
  
  # If there's no given names, there won't be initials either  
  inds <- which(given_names != "")
  family_name <- character(length=length(x))
  family_name[inds] <- str_replace(full_name[inds], given_names[inds], "")
  guessed <- (given_names != initials)
  guessed[which(given_names == "")] <- NA
  
  # Remove extra spaces
  initials <- gsub(" ", "", initials)
  family_name <- gsub(" +", " ", family_name)
  family_name <- gsub("^ (.*) $", "\\1", family_name)
    
  inds2 <- intersect(inds, which(family_name != ""))
  init_name[inds2] <- as.character(paste(initials[inds2], family_name[inds2], sep=" "))
    
  inds2 <- intersect(which(!is.na(full_name)), which(init_name!=""))
  full_name_with_initials[inds2] <- str_replace(x[inds2], full_name[inds2], init_name[inds2])
  
  # Only one upper character in the string -> treated as family name
  inds <- which(str_count(x, "[[:upper:]]")==1)
  initials[inds] <- ""
  family_name[inds] <- str_extract(x[inds], "[^ ]*[[:upper:]][^ ]*")
  full_name[inds] <- str_extract(x[inds], "[^ ]*[[:upper:]][^ ]*")
  full_name_with_initials[inds] <- str_extract(x[inds], "[^ ]*[[:upper:]][^ ]*")
  guessed[inds] <- FALSE
  
  message("... before the other time get_relation_keyword is handled")
  # Relation to named person: widow, inheritors etc. translated to English
  # TODO: Now done twice, but the first time without the knowledge of full_name
  relation <- get_relation_keyword(x, full_name, languages)
  message("... and get_relation_keyword was handled twice")
  
  for (i in nrow(x)) {
    if (is.na(initials[i])) {guessed[i] <- NA}
  }
  
  return (data.frame(orig=orig, initials=initials, family=family_name, full_name=full_name, init_name=full_name_with_initials, guessed=guessed, relation=relation, stringsAsFactors = FALSE))
  
}
