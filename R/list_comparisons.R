#' @title List comparisons
#' @description Lists similar titles from Fennica and Kungliga catalogs
#' @param combined Data frame of both catalogs
#' @param field_names List of field names of exactly matching values
#' @return Data frame of core fields listed by their title
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
list_comparisons <- function (combined, field_names=c("short", "town", "from")) {
    
  l <- (nrow(combined))
  
  # Get matching fields (short title, town, from)
  dups_all <- which(duplicated(combined[field_names],) & duplicated(combined[field_names], fromLast = TRUE))
  
  # Filtering section
  old_inds <- which(combined$from <= 1828)
  dups_all <- intersect(dups_all, old_inds)
  
  # Get first instance of duplicate rows
  dup_f <- which(duplicated(combined[field_names], fromLast=TRUE) & !duplicated(combined[field_names]))
  dup_f <- intersect(dup_f, old_inds)
  
  # Iterate those that are from the Fennica catalog
  fennica_dups <- which(combined$catalog=="fennica")
  fennica_dups <- intersect(fennica_dups, dup_f)
  
  # Prepare for comparison the Kungliga catalog
  kungliga_dups <- which(combined$catalog=="kungliga")
  kungliga_dups <- intersect(kungliga_dups, dups_all)
  
  # Mark indices to remove
  # TODO: fennica_towns is hardcoded now
  fennica_towns <- c("Turku", "Vaasa", "Frankfurt", "Utrecht", "Leipzig", "London", "Stockton", "Vyborg", "Copenhagen", "Leiden", "Paris", "Rostock", "Zary", "St Petersburg", "Helsinki", "Mariehamn", "Kiel", "Berlin", "Haarlem")
  f_remove_inds <- intersect(which(combined$catalog=="fennica"), which(!(combined$town %in% fennica_towns)))
  f_remove_inds <- intersect(dups_all, f_remove_inds)
  k_remove_inds <- intersect(which(combined$catalog=="kungliga"), which(combined$town %in% fennica_towns))
  k_remove_inds <- intersect(dups_all, k_remove_inds)
  remove_inds <- union(k_remove_inds, f_remove_inds)
  combined$remove[remove_inds] <- "potentially remove"
  
  # Prepare a separate data frame for those rows, which are in both catalogs
  combined_friends <- data.frame(catalog_index=character(l),
                                 catalog=character(l), 
                                 author=character(l), 
                                 short=character(l), 
                                 title=character(l), 
                                 town=character(l), 
                                 from=integer(l), 
                                 till=integer(l), 
                                 dissertation=integer(l),
                                 cluster=integer(l), 
                                 cluster_idx=integer(l),
                                 remove=character(l),
                                 stringsAsFactors = FALSE)
  offset <- 1
  cluster_no <- 1
  
  # NB!
  # Currently catches only those duplicates, that are found in both Fennica and Kungliga
  # 1) Iterates through unique Fennica titles (+town, +year)
  # 2) Gets duplicates with counterparts in Kungliga
  for (dup in fennica_dups) {
    rw <- combined[dup,field_names ]
    dups <- merge(combined, rw, by.x = field_names, by.y = field_names)[,names(combined)]

      if (length(which(dups$catalog=="kungliga")) > 0) {  
        new_offset <- (offset + nrow(dups))
        combined_friends[offset:(new_offset-1),1:9] <- dups[,1:9]
        combined_friends$cluster[offset:(new_offset-1)] <- cluster_no
        combined_friends$cluster_idx[offset:(new_offset-1)] <- 1:nrow(dups)
        combined_friends$remove[offset:(new_offset-1)] <- dups$remove
        offset <- new_offset
        cluster_no <- (cluster_no + 1)
    }
  }
  
  # Remove empty rows
  combined_friends <- combined_friends[which(combined_friends$catalog!=""),]

  return (combined_friends)
}
