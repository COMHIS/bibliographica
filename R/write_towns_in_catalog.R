#' @title Write towns in catalog
#' @description Lists & writes publications printed in specific towns from a given catalog
#' @param combined_catalogs Data frame of combined catalogs
#' @param catalog_name String of the catalog whose files are listed
#' @param towns Names of towns that will be listed
#' @param write_to_one_file Boolean indicating if the value should be printed or not
#' @return Boolean Returns always TRUE
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
write_towns_in_catalog <- function(combined_catalogs, catalog_name, towns, write_to_one_file=FALSE) {
  
  # Filter catalog
  to_remove <- which(combined_catalogs$remove!="")
  inds <- as.integer(combined_catalogs$catalog_index[to_remove])
  
  filtered <- combined_catalogs[which(combined_catalogs$catalog==catalog_name),]
  filtered <- filtered[-inds]
  
  filtered <- filtered[which(filtered$from <= 1828),]
  
  
    
    
  if (write_to_one_file) {
    for (town in towns) {
      town_list <- filtered[which(filtered$town==town),]
      write.table(town_list, 
                paste0(file="~/GitHub/fennica/inst/examples/output.tables/", town, "_in_", catalog_name, "_only.csv", 
                       sep ="\t", 
                       quote = FALSE, 
                       row.names = FALSE)
                )
    } 
  } else {
    town_list <- filtered[which(filtered$town %in% towns),]
    write.table(town_list, paste0(file="~/GitHub/fennica/inst/examples/output.tables/Towns_in_", catalog_name, "_only.csv"), sep ="\t", quote = FALSE, row.names = FALSE)
  }
  return (TRUE)
}