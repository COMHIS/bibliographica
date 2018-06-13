#' @title Get dissertation mismatches
#' @description Lists titles which are tagged as dissertations in another catalog, but not in the other one
#' @param combined_catalogs Data frame of combined catalogs
#' @return Data frame of core fields
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
get_dissertation_mismatches <- function(combined_catalogs) {
  
  combined_catalogs$dissertation[which(is.na(combined_catalogs$dissertation))] <- ""
  l <- nrow(combined_catalogs)
  offset <- 1
  
  # Prepare return df
  dissertation_failures  <- data.frame(catalog_index=character(l),
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
  
  # Iterate through catalog clusters
  for (cl in unique(combined_catalogs$cluster)) {
    #diss <- combined_catalogs$dissertation[which(combined_catalogs$cluster==cl & combined_friends$cluster_idx==1)]
    whole_cluster <- combined_catalogs[which(combined_catalogs$cluster==cl),]
    if ((nrow(whole_cluster[which(whole_cluster$diss==""),]) > 0 ) && (nrow(whole_cluster[which(whole_cluster$diss!=""),]) > 0)) {
    
#    if (diss != "") {
      
#      if (nrow(whole_cluster[which(whole_cluster$diss==""),]) > 0) {
        new_offset <- (offset + nrow(whole_cluster))
        
        dissertation_failures[offset:(new_offset-1),] <- whole_cluster
        offset <- new_offset
#      }
#    }
#    if (diss == "") {
      
#      if (nrow(whole_cluster[which(whole_cluster$diss!=""),]) > 0) {
#        new_offset <- (offset + nrow(whole_cluster))
        
#        dissertation_failures[offset:(new_offset-1),] <- whole_cluster
#        offset <- new_offset    }
    }
  }
  # Remove empty rows
  dissertation_failures <- dissertation_failures[which(dissertation_failures$catalog!=""),]
  return (dissertation_failures)
}