#' @title Removes duplicates
#' @description Tags files as to be removed
#' @param df.preprocessed Data frame of combined catalogs
#' @param field_names List of field names of exactly matching values
#' @return Data frame
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
remove_duplicates <- function (df.preprocessed, field_names=c("short_title", "publication_place", "publication_year")) {
    
  l <- (nrow(df.preprocessed))
  
  # Create short titles
  first_word_count <- 5
  pattern = paste0("^([^ ]* ){", first_word_count, "}")
  df.preprocessed$short_title <- str_extract(df.preprocessed$title, pattern=pattern)
  inds <- which(is.na(df.preprocessed$short_title))
  df.preprocessed$short_title[inds] <- df.preprocessed$title[inds]
  df.preprocessed$short_title <- tolower(df.preprocessed$short_title)
  df.preprocessed$short_title <- gsub("[[:punct:]]", "", df.preprocessed$short_title)
  
  # Get matching fields (short title, town, from)
  dups_all <- which(duplicated(df.preprocessed[field_names],) | duplicated(df.preprocessed[field_names], fromLast = TRUE))
  
  # Get first instance of duplicate rows
  dup_f <- which(duplicated(df.preprocessed[field_names], fromLast=TRUE) & !duplicated(df.preprocessed[field_names]))
  df.preprocessed$first_duplicated <- FALSE
  df.preprocessed$first_duplicated[dup_f] <- TRUE
  
  
  # Iterate those that are from the Fennica catalog
  fennica_dups <- df.preprocessed %>% filter(df.preprocessed$catalog=="Fennica")
  fennica_dups <- fennica_dups %>% filter(fennica_dups$first_duplicated==TRUE)
  
  
  # Prepare for comparison the Kungliga catalog
  kungliga_dups <- df.preprocessed[dups_all,] %>% filter(df.preprocessed$catalog[dups_all]=="Kungliga")
  
  # Mark indices to remove
  # TODO: fennica_towns is hardcoded now
  fennica_towns <- c("Turku", "Vaasa", "Frankfurt", "Utrecht", "Leipzig", "London", "Stockton", "Vyborg", "Copenhagen", "Leiden", "Paris", "Rostock", "Zary", "St Petersburg", "Helsinki", "Mariehamn", "Kiel", "Berlin", "Haarlem")
  
  # Get the potential indices for removal, but don't mark them yet in the actual catalog
  f_remove_inds <- intersect(which(df.preprocessed$catalog=="Fennica"), which(!(df.preprocessed$publication_place %in% fennica_towns)))
  f_remove_inds <- intersect(dups_all, f_remove_inds)
  k_remove_inds <- intersect(which(df.preprocessed$catalog=="Kungliga"), which(df.preprocessed$publication_place %in% fennica_towns))
  k_remove_inds <- intersect(dups_all, k_remove_inds)
  remove_inds <- union(f_remove_inds, k_remove_inds)
  
  remove <- character(l)
  remove[remove_inds] <- "remove"
  remove[-remove_inds] <- ""
  
  offset <- 1
  cluster_no <- 1
  
  # NB!
  # Currently catches only those duplicates, that are found in both Fennica and Kungliga
  # 1) Iterates through unique Fennica titles (+town, +year)
  # 2) Gets duplicates with counterparts in Kungliga
  #for (i in 1:nrow(fennica_dups)) {
  for (i in 1:nrow(fennica_dups)) {
    #print("Found dup")
    #print(names(df.preprocessed))
    dup <- fennica_dups[i,field_names]
    #print(dup)
    #print(dup[1,])
    dups <- merge(df.preprocessed, dup, by.x = field_names, by.y = field_names)[,names(df.preprocessed)]
    
    if (length(which(dups$catalog=="Kungliga")) > 0) {  
        #print("Found Kungliga")
        
        new_offset <- (offset + nrow(dups))
        #inds <- which(df.preprocessed[,1:length(dups)] %in% dups$catalog_index)
        inds <- dups$catalog_index[which(dups$catalog=="Fennica")]
        rem_inds <- which(df.preprocessed$catalog=="Fennica" & df.preprocessed$catalog_index %in% inds)
        df.preprocessed$remove[rem_inds] <- remove[rem_inds]
        inds <- dups$catalog_index[which(dups$catalog=="Kungliga")]
        rem_inds <- which(df.preprocessed$catalog=="Kungliga" & df.preprocessed$catalog_index %in% inds)
        df.preprocessed$remove[rem_inds] <- remove[rem_inds]
        #inds <- dups$catalog_index[which(dups$catalog=="Kungliga")]
        #print(inds)
        #combined_friends[offset:(new_offset-1),1:9] <- dups[,1:9]
        #combined_friends$cluster[offset:(new_offset-1)] <- cluster_no
        #combined_friends$cluster_idx[offset:(new_offset-1)] <- 1:nrow(dups)
        #df.preprocessed$remove[offset:(new_offset-1)] <- dups$remove[inds]
        offset <- new_offset
        #cluster_no <- (cluster_no + 1)
    }
  }
  
  df.preprocessed
  # Remove empty rows
  # <- combined_friends[which(combined_friends$catalog!=""),]

  return (df.preprocessed)
}
