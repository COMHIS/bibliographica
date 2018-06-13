#' @title Summary Tables for Geodata
#' @description Generate summary tables from the preprocessed data frame.
#' @param df.preprocessed Preprocessed data.frame to be summarized
#' @param df.orig Original data.frame for comparisons
#' @param output.folder Output folder path
#' @return NULL
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples # generate_summary_tables_geo(df)
#' @keywords utilities
generate_summary_tables_geo <- function (df.preprocessed, df.orig, output.folder = "output.tables") {

  name <- NULL

  # Ensure compatibility			
  df.orig <- df.orig[match(df.preprocessed$original_row, df.orig$original_row),]

  # -------------------------------------------------------------------

  message("Publication country accepted")
  field <- "publication_country"
  s <- write_xtable(df.preprocessed[[field]],
		     paste(output.folder, field, "_accepted.csv", sep = ""),
		     count = TRUE,
		     add.percentages = TRUE)

  message("publication_place accepted")
  tmp <- write_xtable(df.preprocessed[, c("publication_place", "publication_country")],
      filename = paste(output.folder, "publication_place_accepted.csv", sep = ""),
      count = TRUE, sort.by = "publication_place", add.percentages = TRUE)

  # -------------------------------------------------------------------

  message("Publication place conversions")
  nam <- "publication_place"
  o <- as.character(df.orig[[nam]])
  x <- as.character(df.preprocessed[[nam]])  
  inds <- which(!is.na(x) & !(tolower(o) == tolower(x)))
  tmp <- write_xtable(cbind(original = o[inds],
      	 		      polished = x[inds]),
    paste(output.folder, paste("publication_place_conversion_nontrivial.csv", sep = "_"),
    sep = ""), count = TRUE)

  # -------------------------------------------------------------------

  message("Ambiguous publication place harmonization")  
  f <- system.file("extdata/PublicationPlaceSynonymes.csv",
	           package = "bibliographica")
  tab <- read_mapping(f, include.lowercase = T, self.match = T,
      	                 ignore.empty = FALSE,
                         mode = "table", remove.ambiguous = FALSE)
			 
  # Only consider mapping for terms that are present in our data
  tab1 <- subset(tab, name %in% as.character(df.preprocessed$publication_place))
  
  # Then include conversions from our data. This may contain
  # terms that were directly accepted as such and are not on
  # the synonyme table:
  inds <- setdiff(tolower(df.orig$publication_place), tab1$synonyme)
  tab2 <- cbind(
    name = as.character(df.preprocessed$publication_place[inds]),
    synonyme = as.character(tolower(polish_place(df.orig$publication_place[inds], harmonize = TRUE))))
    
  # Combine the data from both tables
  tab <- unique(rbind(tab1, tab2))
  
  # Identify ambiguous mappings
  s <- split(as.character(tab$name), tolower(as.character(tab$synonyme)))
  s <- s[sapply(s, function(x) {length(unique(x))}, USE.NAMES = FALSE) > 1]
  tab <- tab[tab$synonyme %in% names(s),]
  tab <- tab[order(tab$synonyme),]
  
  # Only include those that we have in our data
  tab <- tab[as.character(tab$name) %in% as.character(df.preprocessed$publication_place),]  
  write.table(tab,
    file = paste(output.folder, "publication_place_ambiguous.csv", sep = ""),
  		  sep = ";", quote = F, row.names = F)

  # -------------------------------------------------------------------

  message("publication_place discarded")
  tab <- read_mapping(f, include.lowercase = T, self.match = T,
      	                 ignore.empty = FALSE,
                         mode = "table", remove.ambiguous = FALSE)
  # Only consider terms that are present in our data
  disc <- sort(subset(tab, is.na(name))$synonyme)
  if (length(disc) == 0) {disc <- NULL}
  tmp <- write.csv(disc,
      file = paste(output.folder, "publication_place_discarded.csv", sep = ""),
      quote = FALSE, row.names = FALSE, col.names = FALSE)

  # -------------------------------------------------------------------
  
  message("Publication place todo file")
  f <- system.file("extdata/PublicationPlaceSynonymes.csv",
         package = "bibliographica")
  synonymes <- suppressWarnings(read_mapping(f, include.lowercase = T,
  	         self.match = T, ignore.empty = FALSE,
		 mode = "table", trim = TRUE))
  pl <- polish_place(df.orig$publication_place, remove.unknown = FALSE);
  tmp <- write.table(sort(tolower(setdiff(tolower(pl), tolower(synonymes$name)))),
      file = paste(output.folder, "publication_place_todo.csv", sep = ""),
      	   quote = FALSE, row.names = FALSE, col.names = FALSE)

  # ------------------------------------------------------

  message("Missing country")
  f <- system.file("extdata/PublicationPlaceSynonymes.csv",
         package = "bibliographica")
  syn <- read_mapping(f, include.lowercase = T, self.match = T,
      	   ignore.empty = FALSE, mode = "table")  
  rms <- as.character(syn$synonyme[is.na(as.character(syn$name))])
  tab <- as.character(df.preprocessed$publication_place)[is.na(df.preprocessed$publication_country)]
  # Remove places that have already been explicitly set to unknown
  tab <- setdiff(tab, rms)
  # Then print the rest
  tmp <- write_xtable(tab, filename = "output.tables/publication_place_missingcountry.csv")

  # -------------------------------------------------------------------
  
  message("Ambiguous countries listing")    
  tab <- read.csv(system.file("extdata/reg2country.csv", package = "bibliographica"), sep = ";")
  
  # Cases with explicit mention of ambiguity
  inds2 <- c(grep("Ambiguous", tab$publication_country),
       	   grep("Ambiguous", tab$region),
	   grep("Ambiguous", tab$comment))
  amb2 <- tab[inds2,"region"]
  # Cases with multiple names listed
  inds3 <- grep("\\|", tab$publication_country)
  amb3 <- tab[inds3,"region"]
  # Combine regions with ambiguous country
  amb <- unique(c(as.character(amb2), as.character(amb3)))
  # Pick the table with ambiguous countrues
  tab <- tab[tab$region %in% amb,]
  tab <- tab[order(tab$region),]
  # For brewity, only include the places that we have in our data
  tab <- tab[as.character(tab$region) %in% as.character(df.preprocessed$publication_place),]    
  write.table(tab, file = paste(output.folder, "publication_country_ambiguous.csv", sep = ""), sep = ";", quote = F, row.names = F)

  #----------------------------------------------------

  message("Write places with missing geolocation to file")
  tab <- rev(sort(table(df.preprocessed$publication_place[is.na(df.preprocessed$latitude) | is.na(df.preprocessed$longitude)])))
  tab <- tab[tab > 0]
  tab <- cbind(names(tab), tab)
  colnames(tab) <- c("name", "count")
  write.table(tab, file = paste(output.folder, "absentgeocoordinates.csv", sep = ""), quote = F, row.names = F, sep = "\t")

  return(NULL)
}

