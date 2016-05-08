#' @title Generate summary tables
#' @description Generate summary tables from the preprocessed data frame.
#' @param df.preprocessed Preprocessed data.frame to be summarized
#' @param df.orig Original data.frame for comparisons
#' @param output.folder Output folder path
#' @return NULL
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @importFrom dplyr full_join
#' @export
#' @examples # generate_summary_tables(df)
#' @keywords utilities
generate_summary_tables <- function (df.preprocessed, df.orig, output.folder = "output.tables") {

  # Circumvent build warnings			
  author <- author_name <- author_birth <- author_death <- author_pseudonyme <- author_gender <- name <- NULL
  mean_pagecounts_multivol <- mean_pagecounts_univol <- mean_pagecounts_issue <- NULL

  # Ensure compatibility			
  df.orig <- df.orig[match(df.preprocessed$original_row, df.orig$original_row),]

  message("Write summaries of field entries and count stats for all fields")
  for (field in setdiff(names(df.preprocessed),
    c(names(df.preprocessed)[grep("language", names(df.preprocessed))] , 
    "row.index", "paper.consumption.km2", "publication_decade",
    "publication_year", "publication_year_from", "publication_year_till",
    "pagecount", "obl", "obl.original", "original_row", "dissertation",
    "synodal", "original", "unity", "author_birth", "author_death",
    "gatherings.original", "width.original", "height.original",
    "longitude", "latitude", "page", "item", "publisher.printedfor",
    "publisher", "country", "author_pseudonyme", "publication_place",
    "control_number", "author_name", "author", "area", "width", "height", "gender"))) {

    message(field)

    message("Accepted entries in the preprocessed data")
    s <- write_xtable(df.preprocessed[[field]], paste(output.folder, field, "_accepted.csv", sep = ""), count = TRUE)

    message("Discarded entries")
    if ((field %in% names(df.preprocessed)) && (field %in% names(df.orig))) {
      inds <- which(is.na(df.preprocessed[[field]]))
      original <- as.vector(na.omit(as.character(df.orig[[field]][inds])))
      tmp <- write_xtable(original, paste(output.folder, field, "_discarded.csv", sep = ""), count = TRUE)
    }

    message("Nontrivial conversions")
    if (field %in% names(df.preprocessed) && (field %in% names(df.orig)) && !field %in% c("dimension", "title")) {
      message(field)
      inds <- which(!is.na(df.preprocessed[[field]]))
      original <- as.character(df.orig[[field]][inds])
      polished <- as.character(df.preprocessed[[field]][inds])
      tab <- cbind(original = original, polished = polished)
      # Exclude trivial cases (original == polished exluding cases)
      #tab <- tab[!tab[, "original"] == tab[, "polished"], ]
      tab <- tab[!tolower(tab[, "original"]) == tolower(tab[, "polished"]), ]
      
      tmp <- write_xtable(tab, paste(output.folder, field, "_conversion_nontrivial.csv", sep = ""), count = TRUE)
    }
  }


  message("Conversion summaries")
  originals <- c(publisher = "publisher",
	       country = "publication_place"
	       )
  for (nam in names(originals)) {
    o <- as.character(df.orig[[originals[[nam]]]])
    x <- as.character(df.preprocessed[[nam]])
    inds <- which(!is.na(x) & !(tolower(o) == tolower(x)))
    tmp <- write_xtable(cbind(original = o[inds],
      	 		      polished = x[inds]),
      paste(output.folder, paste(nam, "conversion_nontrivial.csv", sep = "_"),
      sep = ""), count = TRUE)
  }
  
  message("..author conversion")
  o <- gsub("\\]", "", gsub("\\[", "", gsub("\\.+$", "", as.character(df.orig$author_name))))
  x <- as.character(df.orig$author_date)
  inds <- which(!is.na(x) & !(tolower(o) == tolower(x)))
  tmp <- write_xtable(cbind(
    original_name = o[inds],
    original_date = x[inds],
    final_author_id = as.character(df.preprocessed[inds, "author"]),
    final_author_birth = as.character(df.preprocessed[inds, "author_birth"]),
    final_author_death = as.character(df.preprocessed[inds, "author_death"])
			  ),
      paste(output.folder, paste("author_conversion_nontrivial.csv", sep = "_"),
      sep = ""), count = TRUE)

  message("...author")
  # Separate tables for real names and pseudonymes
  tab <- df.preprocessed %>% filter(!author_pseudonyme) %>% select(author, author_gender)
  tmp <- write_xtable(tab,
      paste(output.folder, paste("author_accepted.csv", sep = "_"), sep = ""),
      count = TRUE, sort.by = "author")
  message("...pseudonyme")
  tab <- df.preprocessed %>% filter(author_pseudonyme) %>% select(author)
  tmp <- write_xtable(tab, paste(output.folder, "pseudonyme_accepted.csv", sep = ""),
      	 		   count = TRUE, sort.by = "author")
  message("...publication_place")
  tmp <- write_xtable(df.preprocessed[, c("publication_place", "country")],
      filename = paste(output.folder, "publication_place_accepted.csv", sep = ""),
      count = TRUE, sort.by = "publication_place")

  message("Discard summaries")
  for (nam in setdiff(names(originals), c("country", "publication_place"))) {
    o <- as.character(df.orig[[originals[[nam]]]])
    x <- as.character(df.preprocessed[[nam]])
    inds <- which(is.na(x))
    tmp <- write_xtable(o[inds],
      paste(output.folder, paste(nam, "discarded.csv", sep = "_"), sep = ""),
      count = TRUE)
  }
  message("..author")
  o <- as.character(df.orig[["author_name"]])
  x <- as.character(df.preprocessed[["author"]])
  inds <- which(is.na(x))
  tmp <- write_xtable(o[inds],
      paste(output.folder, paste("author_discarded.csv", sep = "_"), sep = ""),
      count = TRUE)
  # --------------------------------------------

  # Author gender

  message("Accepted entries in the preprocessed data")
  s <- write_xtable(df.preprocessed[["author_gender"]],
         paste(output.folder, "author_gender_accepted.csv", sep = ""),
	 count = TRUE)

  message("Discarded gender")
    if (("author_gender" %in% names(df.preprocessed)) &&
         ("author_gender" %in% names(df.orig))) {
      inds <- which(is.na(df.preprocessed[["author_gender"]]))
      original <- condense_spaces(gsub("\\.", " ", tolower(as.vector(na.omit(as.character(df.preprocessed$author[inds]))))))
      tmp <- write_xtable(original,
        paste(output.folder, "author_gender_discarded.csv", sep = ""),
	count = TRUE)
    }

  message("Author gender tables realized in the final data")
  tab <- data.frame(list(name = pick_firstname(df.preprocessed$author),
                         gender = df.preprocessed$author_gender))
  tab <- tab[!is.na(tab$gender), ] # Remove NA gender

  write_xtable(subset(tab, gender == "male")[,-2],
                 paste(output.folder, "gender_male.csv", sep = ""))
  write_xtable(subset(tab, gender == "female")[,-2],
                 paste(output.folder, "gender_female.csv", sep = ""))

  # Unknown gender
  tmp <- unname(pick_firstname(df.preprocessed$author_name)[is.na(df.preprocessed$author_gender)])
  tmp <- condense_spaces(gsub("\\.", " ", tolower(tmp)))
  inds <- c(nchar(tmp) > 1,
            grep("^[a-z] [a-z]$", tmp),
       	    grep("^[a-z] [a-z] [a-z]$", tmp))
  if (length(inds) > 0) { tmp <- tmp[-inds] }
  tmpg <- write_xtable(tmp, paste(output.folder, "gender_unknown.csv", sep = ""))

  # Unresolved (ambiguous) gender
  tmp <- unname(pick_firstname(df.preprocessed$author_name)[df.preprocessed$author_gender == "ambiguous"])
  tmp <- condense_spaces(gsub("\\.", " ", tolower(tmp)))
  inds <- c(nchar(tmp) > 1,
            grep("^[a-z] [a-z]$", tmp),
       	    grep("^[a-z] [a-z] [a-z]$", tmp))
  if (length(inds) > 0) { tmp <- tmp[-inds] }
  tmpg <- write_xtable(tmp, paste(output.folder, "gender_ambiguous.csv", sep = ""))

  #-------------------------------------------------


  message("Conversion summaries")
  originals <- c(publisher = "publisher",
	       country = "publication_place"
	       )
  for (nam in names(originals)) {
    o <- as.character(df.orig[[originals[[nam]]]])
    x <- as.character(df.preprocessed[[nam]])
    inds <- which(!is.na(x) & !(tolower(o) == tolower(x)))
    tmp <- write_xtable(cbind(original = o[inds],
      	 		      polished = x[inds]),
      paste(output.folder, paste(nam, "conversion_nontrivial.csv", sep = "_"),
      sep = ""), count = TRUE)
  }  


  # --------------------------------------------

  # Pagecount
  o <- as.character(df.orig[["physical_extent"]])
  g <- as.character(df.preprocessed$gatherings)
  x <- as.character(df.preprocessed[["pagecount"]])
  inds <- which(!is.na(x) & !(tolower(o) == tolower(x)))
  tmp <- write_xtable(cbind(gatherings = g[inds],
      	                    original_extent = o[inds],  
      	 		    final_pagecount = x[inds]),
    paste(output.folder, "pagecount_conversion_nontrivial.csv", sep = ""), count = TRUE)
  
  message("Discard summaries")
  inds <- which(is.na(x))
  tmp <- write_xtable(cbind(gatherings = g[inds], physical_extent = o[inds]),
      paste(output.folder, "pagecount_discarded.csv", sep = ""),
      count = TRUE)

  # --------------------------------------------

  message("Conversion: publication year")
  # Publication year
  o <- as.character(df.orig[["publication_time"]])
  x <- df.preprocessed[, c("publication_year", "publication_year_from", "publication_year_till")]
  tab <- cbind(original = o, x)
  tab <- tab[!is.na(tab$publication_year),]
  tmp <- write_xtable(tab,
      paste(output.folder, "publication_year_conversion.csv",
      sep = ""), count = TRUE)
  
  message("Discarded publication year")
  o <- as.character(df.orig[["publication_time"]])
  x <- as.character(df.preprocessed[["publication_year"]])
  inds <- which(is.na(x))
  tmp <- write_xtable(o[inds],
      paste(output.folder, "publication_year_discarded.csv", sep = ""),
      count = TRUE)
  
  # --------------------------------------------

  message("Authors with missing life years")
  tab <- df.preprocessed %>% filter(!is.na(author_name) & (is.na(author_birth) | is.na(author_death))) %>% select(author_name, author_birth, author_death)
  tmp <- write_xtable(tab, paste(output.folder, "authors_missing_lifeyears.csv", sep = ""))
 
  message("Ambiguous authors with many birth years")
  births <- split(df.preprocessed$author_birth, df.preprocessed$author_name)
  births <- births[sapply(births, length) > 0]
  many.births <- lapply(births[names(which(sapply(births, function (x) {length(unique(na.omit(x)))}) > 1))], function (x) {sort(unique(na.omit(x)))})
  dfs <- df.preprocessed[df.preprocessed$author_name %in% names(many.births), c("author_name", "author_birth", "author_death")]
  dfs <- unique(dfs)
  dfs <- dfs %>% arrange(author_name, author_birth, author_death)
  write.table(dfs, paste(output.folder, "author_life_ambiguous.csv", sep = ""), quote = F, sep = "\t", row.names = FALSE)

  # -------------------------------------------------------

  message("Undefined language")
  # Remove "und" from the list ("Undetermined")
  f <- system.file("extdata/language_abbreviations.csv", package = "bibliographica")
  abrv <- read_synonymes(f, include.lowercase = F, self.match = F, ignore.empty = FALSE, mode = "table", sep = "\t")
  # List unique languages that occur in the data
  lang <- unlist(strsplit(df.orig$language, ";"))
  # Remove the known ones (und is Undetermined)
  unknown.lang <- setdiff(lang, c(abrv$synonyme, "und"))
  tmp <- write_xtable(unknown.lang,
	   filename = paste(output.folder, "language_discarded.csv", sep = ""))

  message("Language conversions")
  field = "language"
  original <- as.character(df.orig[[field]])
  polished <- as.character(df.preprocessed[[field]])
  tab <- cbind(original = original, polished = polished)
  tmp <- write_xtable(tab, paste(output.folder, field, "_conversions.csv", sep = ""), count = TRUE)

  # ---------------------------------------------------------

  message("Publication place conversions")
  nam <- "publication_place"
  o <- as.character(df.orig[[nam]])
  x <- as.character(df.preprocessed[[nam]])
  
  inds <- which(!is.na(x) & !(tolower(o) == tolower(x)))
  tmp <- write_xtable(cbind(original = o[inds],
      	 		      polished = x[inds]),
    paste(output.folder, paste("publication_place_conversion_nontrivial.csv", sep = "_"),
    sep = ""), count = TRUE)
  
  message("Publication place discarded")
  inds <- which(is.na(x))
  # Remove places that were already handled in the synonyme table
  # (this could be made optional - useful when actively cleaning up the tables but
  # with new runs it could be beneficial to show everything that is discarded and hence comment this out)
  f <- system.file("extdata/PublicationPlaceSynonymes.csv", package = "bibliographica")
  syn <- read_synonymes(f, include.lowercase = T, self.match = T, ignore.empty = FALSE, mode = "table")
  pl <- tolower(polish_place(o[inds], harmonize = FALSE))
  pl <- setdiff(pl, syn$synonyme)
  
  tmp <- write_xtable(pl,
      paste(output.folder, paste(nam, "discarded.csv", sep = "_"), sep = ""),
      count = TRUE)
  
  message("Accept summaries")
  for (nam in setdiff(names(originals), "publication_place")) {
    x <- as.character(df.preprocessed[[nam]])
    tmp <- write_xtable(x,
      paste(output.folder, paste(nam, "accepted.csv", sep = "_"), sep = ""),
      count = TRUE, sort.by = "Name")
  }
  rms <- as.character(syn$synonyme[is.na(as.character(syn$name))])
  tab <- as.character(df.preprocessed$publication_place)[is.na(df.preprocessed$country)]
  # First remove places that have already been explicitly set to unknown
  tab <- setdiff(tab, rms)
  # Then print the rest
  tmp <- write_xtable(tab, filename = "output.tables/publication_place_missingcountry.csv")

  message("Ambiguous publication place harmonization")  
  f = system.file("extdata/PublicationPlaceSynonymes.csv", package = "bibliographica")
  tab <- read_synonymes(f, include.lowercase = T, self.match = T, ignore.empty = FALSE,
                           mode = "table", remove.ambiguous = FALSE)
  # Only consider terms that are present in our data
  tab1 <- subset(tab, name %in% as.character(df.preprocessed$publication_place))
  # Then also take conversions from our data. This may contain
  # terms that were directly accepted as such as they are not on
  # the synonyme table:
  tab2 <- cbind(name = as.character(df.preprocessed$publication_place),
    synonyme = as.character(tolower(polish_place(df.orig$publication_place, harmonize = FALSE))))
  # Combine the data from both tables
  tab <- unique(rbind(tab1, tab2))
  # Identify ambiguous mappings
  s <- split(as.character(tab$name), tolower(as.character(tab$synonyme)))
  s <- s[sapply(s, function(x) {length(unique(x))}) > 1]
  tab <- tab[tab$synonyme %in% names(s),]
  tab <- tab[order(tab$synonyme),]  
  # Only include those that we have in our data
  tab <- tab[as.character(tab$name) %in% as.character(df.preprocessed$publication_place),]  
  write.table(tab, file = paste(output.folder, "publication_place_ambiguous.csv", sep = ""), sep = ";", quote = F, row.names = F)



  message("Ambiguous countries listing")    
  tab <- read.csv(system.file("extdata/reg2country.csv", package = "bibliographica"), sep = ";")
  # Cases with explicit mention of ambiguity
  inds2 <- c(grep("Ambiguous", tab$country),
       	   grep("Ambiguous", tab$region),
	   grep("Ambiguous", tab$comment))
  amb2 <- tab[inds2,"region"]
  # Cases with multiple names listed
  inds3 <- grep("\\|", tab$country)
  amb3 <- tab[inds3,"region"]
  # Combine regions with ambiguous country
  amb <- unique(c(as.character(amb2), as.character(amb3)))
  # Pick the table with ambiguous countrues
  tab <- tab[tab$region %in% amb,]
  tab <- tab[order(tab$region),]
  # For brewity, only include the places that we have in our data
  tab <- tab[as.character(tab$region) %in% as.character(df.preprocessed$publication_place),]    
  write.table(tab, file = paste(output.folder, "publication_country_ambiguous.csv", sep = ""), sep = ";", quote = F, row.names = F)

  # --------------------------------------------------------

  message("Page counts")
  use.fields <- intersect(c("pagecount", "volnumber", "volcount"), names(df.preprocessed))
  tab <- cbind(original = df.orig$physical_extent, df.preprocessed[, use.fields])
  tmp <- write_xtable(tab, filename = "output.tables/conversions_physical_extent.csv")

  message("Physical dimension info")
  tab <- cbind(original = df.orig$physical_dimension, df.preprocessed[, c("gatherings.original", "width.original", "height.original", "obl.original", "gatherings", "width", "height", "obl", "area")])
  tmp <- write_xtable(tab, filename = "output.tables/conversions_physical_dimension.csv")

  # Accepted / Discarded dimension info
  inds <- which(is.na(df.preprocessed[["area"]]))
  tmp <- write_xtable(
    cbind(original = as.character(df.orig[df.preprocessed$original_row, ]$physical_dimension)[inds], df.preprocessed[inds, c("gatherings", "width", "height", "obl")]),
    paste(output.folder, paste("physical_dimension_incomplete.csv", sep = "_"), sep = ""),
    count = TRUE)

  #-----------------------------------------------------------------------

  # Mean page counts
  # TODO make this more generic; otherwise move completely to ESTC
  mean.pagecounts.multivol <- mean_pagecounts_multivol(df.preprocessed) 
  mean.pagecounts.univol <- mean_pagecounts_univol(df.preprocessed) 
  mean.pagecounts.issue <- mean_pagecounts_issue(df.preprocessed) 
  mean.pagecounts <- full_join(mean.pagecounts.univol, mean.pagecounts.multivol, by = "doc.dimension")
  mean.pagecounts <- full_join(mean.pagecounts, mean.pagecounts.issue, by = "doc.dimension")
  mean.pagecounts$doc.dimension <- factor(mean.pagecounts$doc.dimension,
			      levels = levels(mean.pagecounts.univol$doc.dimension))
  write.table(mean.pagecounts, file = paste(output.folder, "mean_page_counts.csv", sep = ""), quote = F, row.names = F, sep = ",")

  message("Write places with missing geolocation to file")
  tab <- rev(sort(table(df.preprocessed$publication_place[is.na(df.preprocessed$latitude) | is.na(df.preprocessed$longitude)])))
  tab <- tab[tab > 0]
  tab <- cbind(names(tab), tab)
  colnames(tab) <- c("name", "count")
  write.table(tab, file = paste(output.folder, "absentgeocoordinates.csv", sep = ""), quote = F, row.names = F, sep = "\t")

  message("All summary tables generated.")

  return(NULL)
}

