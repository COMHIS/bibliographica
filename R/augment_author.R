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

  # For authors with a unique birth, use this birth year also for documents where
  # birth year not given in the raw data
  df$author_birth <- guess_missing_entries(id = df$author_name, values = df$author_birth)$values
  df$author_death <- guess_missing_entries(id = df$author_name, values = df$author_death)$values

  # print("Add missing author life years")
  if (!is.null(life_info)) {  
    df$author_birth <- add_missing_entries(df, life_info, id = "author_name", field = "author_birth")
    df$author_death <- add_missing_entries(df, life_info, id = "author_name", field = "author_death") 
  }

  print("Unique author identifier by combining name, birth and death years")
  df$author_unique <- author_unique(df, initialize.first = FALSE)

  print("Harmonize ambiguous authors")
  if (!is.null(ambiguous_authors)) {	  	    
    #ambiguous_authors <- ambiguous_authors_table()
    df$author_unique <- harmonize_names(df$author_unique, ambiguous_authors, include.original = FALSE, check.synonymes = FALSE, include.lowercase = FALSE)
  }
  
  print("Correct author living years using the ones from the final harmonized version")	
  inds <- which(!is.na(df$author_unique))
  dfs <- df[inds,] 
  dfs <- select(dfs, author_unique)
  dfs <- dfs %>% separate(col = author_unique, sep = c("\\("), into = c("name", "years"))
  years <- gsub("\\)", "", dfs$years)
  years <- sapply(years, function (x) {if (length(grep("-", unlist(strsplit(x, "")))) == 1) {return(strsplit(x, "-"))} else {return(strsplit(x, " "))}})
  birth <- as.numeric(gsub("-$", "", sapply(years, function (x) {x[[1]]})))
  death <- as.numeric(sapply(years, function (x) {if (length(x) >=2) x[[2]] else NA}))
  df$author_birth[inds] <- birth
  df$author_death[inds] <- death

  df$author <- df$author_unique
  df$author[df$author == "NA, NA"] <- NA
  df$author[grep("^NA, NA ", df$author)] <- NA  
  df$author_unique <- NULL

  df

}