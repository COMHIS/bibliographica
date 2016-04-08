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
  author_name <- author_birth <- author_death <- author_pseudonyme <- NULL
  mean_pagecounts_multivol <- mean_pagecounts_univol <- mean_pagecounts_issue <- NULL

  # Ensure compatibility			
  df.orig <- df.orig[df.preprocessed$original_row,]

  message("Write summaries of field entries and count stats for all fields")
  for (field in setdiff(names(df.preprocessed), c(names(df.preprocessed)[grep("language", names(df.preprocessed))], "row.index", "paper.consumption.km2", "publication_decade", "publication_year", "pagecount", "obl", "obl.original", "original_row", "dissertation", "synodal", "original", "unity", "author_birth", "author_death", "gatherings.original", "width.original", "height.original", "longitude", "latitude", "page", "item", "publisher.printedfor", "publisher", "country", "author_pseudonyme", "publication_place"))) {

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
    if (field %in% names(df.preprocessed) && (field %in% names(df.orig)) && !field == "dimension") {
      print(field)
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

  message("Discarded publication place")
  nam <- "publication_place"
  o <- as.character(df.orig[[nam]])
  x <- as.character(df.preprocessed[[nam]])
  inds <- which(is.na(x))
save(o, inds, file = "~/tmp/tmp.RData")  
  tmp <- write_xtable(polish_place(o[inds], harmonize = FALSE),
      paste(output.folder, paste(nam, "discarded.csv", sep = "_"), sep = ""),
      count = TRUE)
  
  message("Conversion summaries")
  originals <- c(publisher = "publisher",
	       pagecount = "physical_extent",
	       publication_place = "publication_place",
	       country = "publication_place",
	       publication_year = "publication_time",
	       author = "author_name",
	       author_gender = "author_name"
	       #title = "title"	# Very large summaries
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

  message("Accept summaries")
  for (nam in names(originals)) {
    x <- as.character(df.preprocessed[[nam]])
    tmp <- write_xtable(x,
      paste(output.folder, paste(nam, "accepted.csv", sep = "_"), sep = ""),
      count = TRUE)
  }

  message("Discard summaries")
  for (nam in setdiff(names(originals), c("country", "publication_place"))) {
    o <- as.character(df.orig[[originals[[nam]]]])
    x <- as.character(df.preprocessed[[nam]])
    inds <- which(is.na(x))
    tmp <- write_xtable(o[inds],
      paste(output.folder, paste(nam, "discarded.csv", sep = "_"), sep = ""),
      count = TRUE)
  }

  message("Automated summaries done.")

  message("Author pseudonymes")
  tab <- df.preprocessed %>% filter(author_pseudonyme) %>% select(author_name)
  tmp <- write_xtable(tab, paste(output.folder, "author_pseudonymes.csv", sep = ""), count = TRUE)

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

  message("Undefined language")
  tmp <- write_xtable(as.character(df.orig$language[df.preprocessed$language.undetermined]), filename = "output.tables/language_unidentified.csv")

  message("No country mapping - output the harmonized names")
  tab <- as.character(df.preprocessed$publication_place)[is.na(df.preprocessed$country)]
  tmp <- write_xtable(tab, filename = "output.tables/publication_place_missingcountry.csv")

  message("Page counts")
  use.fields <- intersect(c("pagecount", "volnumber", "volcount"), names(df.preprocessed))
  tab <- cbind(original = df.orig$physical_extent, df.preprocessed[, use.fields])
  tmp <- write_xtable(tab, filename = "output.tables/conversions_physical_extent.csv")

  print("Physical dimension info")
  tab <- cbind(original = df.orig$physical_dimension, df.preprocessed[, c("gatherings.original", "width.original", "height.original", "obl.original", "gatherings", "width", "height", "obl", "area")])
  tmp <- write_xtable(tab, filename = "output.tables/conversions_physical_dimension.csv")

  # Accepted / Discarded dimension info
  inds <- which(is.na(df.preprocessed[["area"]]))
  tmp <- write_xtable(
    cbind(original = as.character(df.orig[df.preprocessed$original_row]$physical_dimension)[inds], df.preprocessed[inds, c("gatherings", "width", "height", "obl")]),
    paste(output.folder, paste("physical_dimension_incomplete.csv", sep = "_"), sep = ""),
    count = TRUE)

  message("Write the mapped author genders in tables")
  tab <- data.frame(list(name = df.preprocessed$author, gender = df.preprocessed$author_gender))
  tab <- tab[!is.na(tab$gender), ] # Remove NA gender

  write_xtable(subset(tab, gender == "male")[,-2], paste(output.folder, "gender_male.csv", sep = ""))
  write_xtable(subset(tab, gender == "female")[,-2], paste(output.folder, "gender_female.csv", sep = ""))
  write_xtable(unname(pick_firstname(df.preprocessed$author_name)[is.na(df.preprocessed$author_gender)]), paste(output.folder, "gender_unknown.csv", sep = ""))

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

  message("Ambiguous publication place harmonization")  
  tab = read.csv(system.file("extdata/PublicationPlaceSynonymes.csv", package = "bibliographica"), sep = ";")
  s <- split(as.character(tab$name), as.character(tab$synonyme))
  s <- s[sapply(s, function(x) {length(unique(x))}) > 1]
  tab <- tab[tab$synonyme %in% names(s),]
  tab <- tab[order(tab$synonyme),]
  # Only include those that we have in our data
  tab <- tab[as.character(tab$name) %in% as.character(df.preprocessed$publication_place),]  
  write.table(tab, file = paste(output.folder, "publication_place_ambiguous.csv", sep = ""), sep = ";", quote = F, row.names = F)


  message("Ambiguous countries listing")    
  tab <- read.csv(system.file("extdata/reg2country.csv", package = "bibliographica"), sep = ";")
  s <- split(as.character(tab$country), as.character(tab$region))
  inds1 <- which((sapply(s, function(x) {length(unique(x))}) > 1) | (s == "Ambiguous"))
  amb1 = names(s[inds1])
  
  inds2 = c(grep("Ambiguous", tab$country),
       	   grep("Ambiguous", tab$region),
	   grep("Ambiguous", tab$comment))
  amb2 = tab[inds2,"region"]
  amb <- unique(c(amb1, amb2))
  tab <- tab[tab$region %in% amb,]
  tab <- tab[order(tab$region),]
  # Only include those that we have in our data
  tab <- tab[as.character(tab$region) %in% as.character(df.preprocessed$publication_place),]    
  write.table(tab, file = paste(output.folder, "publication_country_ambiguous.csv", sep = ""), sep = ";", quote = F, row.names = F)

  message("All summary tables generated.")

  return(NULL)
}

