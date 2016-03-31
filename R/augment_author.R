#' @title Augment author info
#' @description Estimate missing entries in author info
#' @param df data.frame with author_birth, author_death and author_name
#' @param life_info Additional author life years info table
#' @param ambiguous_authors Author synonyme table
#' @return Augmented data.frame
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @importFrom dplyr select
#' @importFrom tidyr separate 
#' @examples # augment_author_life(df)
#' @details Augments author life years (author_birth and author_death) based on information from other entries of the same author where the info is available. Also supports manually provided information tables. Adds author_unique field which combines full author name and life years to provide a unique author identifier. Finally harmonizes ambiguous author names based on synonyme table.
#' @keywords utilities
augment_author <- function (df, life_info = NULL, ambiguous_authors = NULL) {

  author <- NULL

  message("Augment author life years")	       
  # For authors with a unique birth, use this birth year also when
  # birth year not given in the raw data
  df$author_birth <- guess_missing_entries(id = df$author_name, values = df$author_birth)$values
  df$author_death <- guess_missing_entries(id = df$author_name, values = df$author_death)$values

  # print("Add missing author life years from the predefined table")
  if (!is.null(life_info)) {  
    df$author_birth <- add_missing_entries(df, life_info, id = "author_name", field = "author_birth")
    df$author_death <- add_missing_entries(df, life_info, id = "author_name", field = "author_death") 
  }

  message("Add pseudonyme indicator field")
  pseudo1 <- as.character(read.csv(system.file("extdata/stopwords_pseudonymes.csv", package = "bibliographica"), sep = "\t")[,1])
  pseudo2 <- as.character(read.csv(system.file("extdata/names/pseudonymes/first.csv", package = "bibliographica"), sep = "\t")[,1])
  pseudo3 <- as.character(read.csv(system.file("extdata/names/pseudonymes/last.csv", package = "bibliographica"), sep = "\t")[,1])
  pseudo <- sort(unique(c(pseudo1, pseudo2, pseudo3)))
  df$author_pseudonyme <- tolower(df$author_name) %in% pseudo

  message("Unique author identifier by combining name, birth and death years")
  df$author <- author_unique(df, initialize.first = FALSE)

  message("Harmonize ambiguous authors")
  if (!is.null(ambiguous_authors)) {	  	    
    df$author <- harmonize_names(df$author, ambiguous_authors, include.original = FALSE, check.synonymes = FALSE, include.lowercase = FALSE)
  }
  df$author[grep("^NA, NA", df$author)] <- NA  

  message("Fix author life years using the ones from the final harmonized version")
  dfa.orig <- df
  dfa.uniq <- unique(dfa.orig)
  
  # Entry IDs
  id.orig <- apply(dfa.orig, 1, function (x) {paste(as.character(x), collapse = "")})
  id.uniq <- apply(dfa.uniq, 1, function (x) {paste(as.character(x), collapse = "")})
  match.inds <- match(id.orig, id.uniq)
  rm(id.orig)  

  inds <- which(!is.na(dfa.uniq$author))
  dfa.uniqs <- dfa.uniq[inds,] 
  dfa.uniqs <- select(dfa.uniqs, author)
  dfa.uniqs <- dfa.uniqs %>% separate(col = author, sep = c("\\("), into = c("name", "years"))
  years <- gsub("\\)", "", dfa.uniqs$years)

  message(".. retrieving the years ..")
  years <- sapply(years, function (x) {if (length(grep("-", unlist(strsplit(x, "")))) == 1) {return(strsplit(x, "-"))} else {return(strsplit(x, " "))}})
  birth <- as.numeric(gsub("-$", "", sapply(years, function (x) {x[[1]]})))
  death <- as.numeric(sapply(years, function (x) {if (length(x) > 1) x[[2]] else NA}))
  dfa.uniq$author_birth[inds] <- as.numeric(as.character(birth))
  dfa.uniq$author_death[inds] <- as.numeric(as.character(death))

  dfa.uniq[match.inds,]

}