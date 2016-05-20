#' @title Augment Author Info
#' @description Estimate missing entries in author info.
#' @param df data.frame with author_birth, author_death and author_name
#' @param life_info Additional author life years info table
#' @param ambiguous_authors Author synonyme table
#' @return Augmented data.frame
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples # augment_author_life(df)
#' @details Augments author life years (author_birth and author_death) based on information from other entries of the same author where the info is available. Also supports manually provided information tables. Adds author_unique field which combines full author name and life years to provide a unique author identifier. Finally harmonizes ambiguous author names based on synonyme table.
#' @keywords utilities
augment_author <- function (df, life_info = NULL, ambiguous_authors = NULL) {

  author <- starts_with <- NULL
  
  # Consider only unique entries to speed up
  dfa <- df %>% select(starts_with("author"))
  dfa.uniq <- unique(as_data_frame(dfa))
  dfa.uniq$author_name <- as.character(dfa.uniq$author_name)

  message("..entry IDs for unique entries")
  dfa$id <- apply(dfa, 1, function (x) {paste(as.character(x), collapse = "")})
  dfa.uniq$id <- apply(dfa.uniq, 1, function (x) {paste(as.character(x), collapse = "")})  
  # Form match indices and remove unnecessary vars to speed up
  match.inds <- match(dfa$id, dfa.uniq$id)
  rm(dfa); dfa.uniq$id <- NULL

  message("Augment author life years")	       
  # For authors with a unique birth, use this birth year also when
  # birth year not given in the raw data
  dfa.uniq$author_birth <- guess_missing_entries(id = dfa.uniq$author_name, values = dfa.uniq$author_birth)$values
  dfa.uniq$author_death <- guess_missing_entries(id = dfa.uniq$author_name, values = dfa.uniq$author_death)$values

  # print("Add missing author life years from the predefined table")
  if (!is.null(life_info)) {  
    dfa.uniq$author_birth <- add_missing_entries(dfa.uniq, life_info, id = "author_name", field = "author_birth")
    dfa.uniq$author_death <- add_missing_entries(dfa.uniq, life_info, id = "author_name", field = "author_death") 
  }

  message("Add pseudonyme field")
  pseudo <- get_pseudonymes()
  dfa.uniq$author_pseudonyme <- tolower(dfa.uniq$author_name) %in% pseudo
  # Polish pseudonymes
  pse <- dfa.uniq$author_name[dfa.uniq$author_pseudonyme]
  pse <- gsub("\\,+", " ", pse)
  pse <- gsub("\\.+", "", pse)
  pse <- gsub("\\-+", " ", pse)    
  pse <- condense_spaces(pse)
  dfa.uniq$author_name[dfa.uniq$author_pseudonyme] <- pse

  message("Unique author identifier by combining name, birth and death years")
  author <- dfa.uniq$author_name
  # Add years only for real persons, not for pseudonymes
  author[which(!dfa.uniq$author_pseudonyme)] <- author_unique(dfa.uniq[which(!dfa.uniq$author_pseudonyme),], initialize.first = FALSE)
  dfa.uniq$author <- author
  rm(author)
  
  message("Harmonize ambiguous authors, including pseudonymes")
  if (!is.null(ambiguous_authors)) {	  	    
    dfa.uniq$author <- map(dfa.uniq$author, ambiguous_authors)
  }
  dfa.uniq$author[grep("^NA, NA", dfa.uniq$author)] <- NA  

  message("Fix author life years using the ones from the final harmonized version")
  message(".. retrieving the years ..")
  a <- dfa.uniq$author
  a <- strsplit(a, "\\(")
  len <- sapply(a, length, USE.NAMES = FALSE)
  a[len < 2] <- NA
  a[which(len == 2)] <- sapply(a[which(len == 2)], function (x) {x[[2]]}, USE.NAMES = FALSE)
  years <- gsub("\\)", "", a)
  rm(len);rm(a)
  
  message(".. splitting the years ..")
  years2 <- polish_years(years)
  spl <- strsplit(years, "-")
  len <- sapply(spl, length, USE.NAMES = FALSE)
  inds <- which(len >= 3)
  # 1300-1400-1500 case separately
  # take the range
  years2[inds,] <-  matrix(sapply(spl[inds], function (x) {range(na.omit(as.numeric(x)))}, USE.NAMES = FALSE), ncol = 2)
  # Then if birth = death, remove death
  years2[which(years2$from == years2$till), "till"] <- NA

  dfa.uniq$author_birth <- years2$from
  dfa.uniq$author_death <- years2$till

  # Replace the original entries with the updated ones
  df[, colnames(dfa.uniq)] <- dfa.uniq[match.inds,]

  df

}

