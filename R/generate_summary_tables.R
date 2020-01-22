#' @title Summary Tables
#' @description Generate summary tables from the preprocessed data frame.
#' @param df.preprocessed Preprocessed data.frame to be summarized
#' @param df.orig Original data.frame for comparisons
#' @param output.folder Output folder path
#' @return NULL
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples # generate_summary_tables(df)
#' @keywords utilities
generate_summary_tables <- function (df.preprocessed, df.orig, output.folder = "output.tables") {

  # Circumvent build warnings			
  df <- author <- author_name <- author_birth <- author_death <- author_pseudonyme <- author_gender <- name <- NULL
  mean_pagecounts_multivol <- mean_pagecounts_singlevol <- mean_pagecounts_issue <- NULL
  obl <- NULL
  tally <- NULL
  desc <- NULL
  original_name <- NULL
  original_date <- NULL
  final_author_id <- NULL
  final_author_birth <- NULL
  final_author_death <- NULL
  publisher <- NULL
  gatherings <- NULL
  original_extent <- NULL
  final_pagecount <- NULL
  physical_extent <- NULL
  estimated_pagecount <- NULL
  publication_year <- NULL
  publication_interval <- NULL
  publication_interval_from <- NULL
  publication_interval_till <- NULL
  width <- NULL
  height <- NULL

  # Ensure compatibility			
  df.orig <- df.orig[match(df.preprocessed$original_row, df.orig$original_row),]

  message("Write summaries of field entries and count stats for all fields")
  for (field in setdiff(names(df.preprocessed),
    c(names(df.preprocessed)[grep("language", names(df.preprocessed))] , 
    "row.index", "paper", "publication_decade",
    "publication_year", "publication_year_from", "publication_year_till",
    "publication_interval",
    "publication_interval_from",
    "publication_interval_till",        
    "subject_topic", 
    "pagecount", "obl", "obl.original", "original_row", "dissertation",
    "synodal", "language", "original", "unity", "author_birth", "author_death",
    "gatherings.original", "width.original", "height.original",
    "longitude", "latitude", "page", "item", "publisher.printedfor",
    "publisher", "author_pseudonyme", 
    "control_number", "system_control_number",
    "author_name", "author", "area", "width", "height", "gender"))) {

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
      tab <- tab[!tolower(tab[, "original"]) == tolower(tab[, "polished"]), ]
      tab <- as.data.frame(tab)
      x <- tab %>% group_by(original, polished) %>%
                   tally() %>%
		   arrange(desc(n))
      
      tmp <- write.table(x, file = paste(output.folder, field, "_conversion_nontrivial.csv", sep = ""),
                            quote = FALSE,
			    row.names = FALSE
      	     		    	 )

    }
  }

  # --------------------------------------------------------------

  message("..author conversion")
  o <- gsub("\\]", "", gsub("\\[", "", gsub("\\.+$", "", as.character(df.orig$author_name))))
  x <- as.character(df.orig$author_date)
  inds <- which(!is.na(x) & !(tolower(o) == tolower(x)))

  x1 <- as.character(df.preprocessed[inds, "author"])
  x2 <- as.character(df.preprocessed[inds, "author_birth"])
  x3 <- as.character(df.preprocessed[inds, "author_death"])

  li <- list(
         original_name = o[inds],
         original_date = x[inds],
       final_author_id = x1,
    final_author_birth = x2,
    final_author_death = x3
			  )

  df <- as.data.frame(li)

  x <- df %>%
         group_by(original_name, original_date, final_author_id, final_author_birth, final_author_death) %>%
         tally() %>%
	 arrange(desc(n))

  write.table(x, 
      file = paste(output.folder, paste("author_conversion_nontrivial.csv", sep = "_"), sep = ""),
      quote = FALSE,
      row.names = FALSE
  )

  # -----------------------------------------------------

   message("subject_topic")
   field <- "subject_topic"
   entries <- unlist(strsplit(as.character(df.preprocessed[[field]]), ";"), use.names = FALSE)
   s <- write_xtable(entries, paste(output.folder, field, "_accepted.csv", sep = ""), count = TRUE)

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
     tab <- tab[!tolower(tab[, "original"]) == tolower(tab[, "polished"]), ] 
     tmp <- write_xtable(tab, paste(output.folder, field, "_conversion_nontrivial.csv", sep = ""), count = TRUE)
   }
  
  # -----------------------------------------------------

  message("Author")
  # Separate tables for real names and pseudonymes
  tab <- df.preprocessed %>% filter(!author_pseudonyme) %>%
      	 		     select(author, author_gender)
			     
  tmp <- write_xtable(tab,
      paste(output.folder, paste("author_accepted.csv", sep = "_"), sep = ""),
      count = FALSE, sort.by = "author")

  # -------------------------------------------------      

  message("Pseudonyme")
  tab <- df.preprocessed %>% filter(author_pseudonyme) %>% select(author)
  tmp <- write_xtable(tab, paste(output.folder, "pseudonyme_accepted.csv", sep = ""),
      	 		   count = TRUE, sort.by = "author")

  # ----------------------------------------------------

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
  inds <- which(is.na(df.preprocessed[["author_gender"]]))
  dg <- condense_spaces(gsub("\\.", " ", tolower(as.vector(na.omit(as.character(df.preprocessed$author[inds]))))))
  inds <- grep("^[a-z] [a-z]-[a-z]$", dg)  
  if (length(inds)>0) {
    dg <- dg[-inds]
  }
  tmp <- write_xtable(dg,
        paste(output.folder, "author_gender_discarded.csv", sep = ""),
	count = TRUE)
 
  message("Author gender tables realized in the final data")
  tab <- data.frame(list(name = pick_firstname(df.preprocessed$author_name),
                         gender = df.preprocessed$author_gender))
  tab <- tab[!is.na(tab$gender), ] # Remove NA gender

  write_xtable(subset(tab, gender == "male")[,-2],
                 paste(output.folder, "gender_male.csv", sep = ""))
  write_xtable(subset(tab, gender == "female")[,-2],
                 paste(output.folder, "gender_female.csv", sep = ""))

  # Unknown gender
  tmp <- unname(pick_firstname(df.preprocessed$author_name)[is.na(df.preprocessed$author_gender)])
  tmp <- condense_spaces(gsub("\\.", " ", tolower(tmp)))
  inds <- unique(
            c(which(nchar(tmp) == 1),
            grep("^[a-z] [a-z] von$", tmp),
            grep("^[a-z]{1,2} [a-z]{1,2}$", tmp),
       	    grep("^[a-z] [a-z] [a-z]$", tmp)))
  if (length(inds) > 0) { tmp <- tmp[-inds] }
  tmpg <- write_xtable(tmp, paste(output.folder, "gender_unknown.csv", sep = ""))

  # Unresolved (ambiguous) gender
  tmp <- unname(pick_firstname(df.preprocessed$author_name)[df.preprocessed$author_gender == "ambiguous"])
  tmp <- condense_spaces(gsub("\\.", " ", tolower(tmp)))
  inds <- c(which(nchar(tmp) == 1),
            grep("^[a-z]{1,2} [a-z]{1,2}$", tmp),
            grep("^[a-z]{1,2} [a-z]{1,2} von$", tmp),
	    grep("^von$", tmp),	    
            grep("^[a-z] von$", tmp),	    
       	    grep("^[a-z] [a-z] [a-z]$", tmp))
  if (length(inds) > 0) { tmp <- tmp[-inds] }
  tmpg <- write_xtable(tmp, paste(output.folder, "gender_ambiguous.csv", sep = ""))

  #-------------------------------------------------

  ## PUBLISHER
  # The result is visible at
  # https://github.com/COMHIS/fennica/blob/master/inst/examples/publisher.md
  # and that page is generated from
  # inst/extdata/publisher.Rmd

   message("Accepted publishers")
   x <- df.preprocessed[, c("publisher", "self_published")] %>%
          group_by(publisher, self_published) %>%
	  tally() %>%
	  arrange(desc(n))
   
   s <- write.table(x,
     	  file = paste(output.folder, field, "_accepted.csv", sep = ""),
	  quote = FALSE, row.names = FALSE
	  )

   message("Discarded publishers")
   if ((field %in% names(df.preprocessed)) && (field %in% names(df.orig))) {
      inds <- which(is.na(df.preprocessed[[field]]))
      original <- as.vector(na.omit(as.character(df.orig[[field]][inds])))
      # Remove trivial cases to simplify output
      inds <- c(grep("^\\[*s\\.*n\\.*\\]*[0-9]*\\.*$", tolower(original)),
      	        grep("^\\[*s\\.*n\\.*\\[*[0-9]*$", tolower(original)))
		
      if (length(inds) > 0) {		
        original <- original[-inds]
      }
      tmp <- write_xtable(original, paste(output.folder, field, "_discarded.csv", sep = ""), count = TRUE)
   }

  message("publisher conversions")
    o <- as.character(df.orig[[field]])
    x <- as.character(df.preprocessed[[field]])
    inds <- which(!is.na(x) & !(tolower(o) == tolower(x)))
    tmp <- write_xtable(cbind(original = o[inds],
      	 		      polished = x[inds]),
      paste(output.folder, paste(field, "conversion_nontrivial.csv", sep = "_"),
      sep = ""), count = TRUE)
    
  # --------------------------------------------


  ## CORPORATE
  # The result is visible at
  # https://github.com/COMHIS/fennica/blob/master/inst/examples/publisher.md
  # and that page is generated from
  # inst/extdata/publisher.Rmd

   message("Accepted corporates")
   field <- "corporate"

   s <- write_xtable(df.preprocessed[[field]],
     	  paste(output.folder, field, "_accepted.csv", sep = ""),
	  count = TRUE)

   message("Discarded corporates")
   if ((field %in% names(df.preprocessed)) && (field %in% names(df.orig))) {
      inds <- which(is.na(df.preprocessed[[field]]))
      original <- as.vector(na.omit(as.character(df.orig[[field]][inds])))
      # Remove trivial cases to simplify output
      inds <- c(grep("^\\[*s\\.*n\\.*\\]*[0-9]*\\.*$", tolower(original)),
      	        grep("^\\[*s\\.*n\\.*\\[*[0-9]*$", tolower(original)))
		
      if (length(inds) > 0) {		
        original <- original[-inds]
      }
      tmp <- write_xtable(original, paste(output.folder, field, "_discarded.csv", sep = ""), count = TRUE)
   }

  message("corporate conversions")
  nam <- "corporate"
    o <- as.character(df.orig[[nam]])
    x <- as.character(df.preprocessed[[nam]])
    inds <- which(!is.na(x) & !(tolower(o) == tolower(x)))
    tmp <- write_xtable(cbind(original = o[inds],
      	 		      polished = x[inds]),
      paste(output.folder, paste(nam, "conversion_nontrivial.csv", sep = "_"),
      sep = ""), count = TRUE)
      
  # ----------------------------

  message("Pagecount  conversions")
  o <- as.character(df.orig[["physical_extent"]])
  g <- as.character(df.preprocessed$gatherings)
  x <- as.character(df.preprocessed[["pagecount"]])

  # Do not show the estimated ones,
  # just the page counts that were originally available
  #x2 <- rep("", nrow(df.preprocessed));
  # x2[is.na(df.preprocessed[["pagecount.orig"]])] <- "estimate"
  inds <- which(!is.na(x) & !(tolower(o) == tolower(x)) &
                !is.na(df.preprocessed[["pagecount.orig"]]))
		
  tmp <- cbind(gatherings = g[inds],
      	                    original_extent = o[inds],  
      	 		    final_pagecount = x[inds]
			    )
  xx <- as.data.frame(tmp) %>% group_by(gatherings, original_extent, final_pagecount) %>%
                              tally() %>%
			      arrange(desc(n))

  write.table(xx, 
    file = paste(output.folder, "pagecount_conversions.csv", sep = ""),
       quote = FALSE,
       row.names = FALSE)

  # ----------------------------------------------

  message("Discard summaries")
  inds <- which(is.na(df.preprocessed$pagecount.orig))

  tmp <- cbind(
      	   gatherings = g[inds],	   
	   physical_extent = o[inds],
	   estimated_pagecount = x[inds]
	   )
  x <- as.data.frame(tmp) %>% group_by(gatherings, physical_extent, estimated_pagecount) %>%
                              tally() %>%
			      arrange(desc(n))

  write.table(x, file=paste(output.folder, "pagecount_discarded.csv", sep = ""), quote = FALSE, row.names = FALSE)

  # --------------------------------------------

  message("Conversion: publication year")
  # Publication year
  o <- gsub("\\.$", "", as.character(df.orig[["publication_time"]]))
  x <- df.preprocessed[, c("publication_year", "publication_year_from", "publication_year_till")]
  tab <- cbind(original = o, x)
  tab <- tab[!is.na(tab$publication_year),]
  xx <- as.data.frame(tab) %>% group_by(original, publication_year) %>% tally() %>% arrange(desc(n))
  
  tmp <- write.table(xx,
      file = paste(output.folder, "publication_year_conversion.csv",
      sep = ""), quote = FALSE, row.names = FALSE)
  
  message("Discarded publication year")
  o <- as.character(df.orig[["publication_time"]])
  x <- as.character(df.preprocessed[["publication_year"]])
  inds <- which(is.na(x))
  tmp <- write_xtable(o[inds],
      paste(output.folder, "publication_year_discarded.csv", sep = ""),
      count = TRUE)

  # --------------------------------------------

  message("Accepted publication frequency")
  if ("publication_frequency_text" %in% names(df.preprocessed)) {

     publication_frequency_annual <- NULL

     dfp <- df.preprocessed[, c("publication_frequency_text",
			        "publication_frequency_annual")]
     # Remove NA			 
     inds <- is.na(dfp$publication_frequency_text) &
     	     is.na(dfp$publication_frequency_annual)
     dfp <- dfp[!inds,]
     xx <- dfp %>% group_by(publication_frequency_text, publication_frequency_annual) %>%
                   tally() %>%
		   arrange(desc(publication_frequency_annual))
     
    tmp <- write.table(xx,
      file = paste(output.folder, "publication_frequency_accepted.csv", sep = ""),
      quote = FALSE, row.names = FALSE
      )
  
    message("Conversion: publication frequency")
    o <- cbind(original_frequency = condense_spaces(tolower(gsub("\\.$", "", as.character(df.orig[["publication_frequency"]])))),
               original_interval = condense_spaces(tolower(gsub("\\.$", "", as.character(df.orig[["publication_interval"]])))),
               original_time = condense_spaces(tolower(gsub("\\.$", "", as.character(df.orig[["publication_time"]]))))
       )
       
    x <- df.preprocessed[, c("publication_frequency_text", "publication_frequency_annual")]
    tab <- cbind(x, o)
    tab$publication_frequency_annual <- round(tab$publication_frequency_annual, 2)

    tab <- tab[which(!rowMeans(is.na(tab[, 1:3])) == 1),] # Remove NA cases
    xx <- tab %>% group_by(publication_frequency_text, publication_frequency_annual) %>%
                  tally() %>%
		  arrange(desc(n))

    tmp <- write.table(xx,
      file = paste(output.folder, "publication_frequency_conversion.csv",
      sep = ""), quote = FALSE, row.names = FALSE)
  
    message("Discarded publication frequency")
    o <- as.character(df.orig[["publication_frequency"]])
    x1 <- as.character(df.preprocessed[["publication_frequency_annual"]])
    x2 <- as.character(df.preprocessed[["publication_frequency_text"]])    
    inds <- which(is.na(x1) & is.na(x2))
    tmp <- write_xtable(o[inds],
      paste(output.folder, "publication_frequency_discarded.csv", sep = ""),
      count = TRUE)
      
  }

  # --------------------------------------------

  message("Conversion: publication interval")
  if ("publication_interval_from" %in% names(df.preprocessed)) {
  
    # Publication interval
    o <- tolower(gsub("\\.$", "", as.character(df.orig[["publication_interval"]])))
    x <- df.preprocessed[, c("publication_interval_from", "publication_interval_till")]
    tab <- cbind(original = o, x)
    tab <- tab[!is.na(tab$publication_interval_from) | !is.na(tab$publication_interval_till),]
    xx <- tab %>% group_by(original) %>% tally() %>% arrange(desc(n))
    tmp <- write.table(xx,
      paste(output.folder, "publication_interval_conversion_nontrivial.csv",
      sep = ""), quote = FALSE, row.names = FALSE)
  
    message("Discarded publication interval")
    o <- df.orig[, c("publication_interval", "publication_time", "publication_frequency")]
    o$publication_time <- gsub("^\\[", "", gsub("\\]$", "", gsub("\\.$", "", o$publication_time)))
    x <- df.preprocessed[,c("publication_interval_from", "publication_interval_till")]
    x2 <- df.preprocessed[, c("publication_frequency_annual", "publication_frequency_text")]
    inds <- which(rowSums(is.na(x)) == 2 & rowSums(is.na(x2)) == 2 )
    o <- o[inds,]
    inds <- is.na(unlist(o[,1])) & grepl("^[0-9]+$", unlist(o[, 2])) & is.na(unlist(o[,3]))
    xx <- o[!inds,] %>% group_by(publication_interval) %>% tally() %>% arrange(desc(n))
    tmp <- write.table(xx,
      file = paste(output.folder, "publication_interval_discarded.csv", sep = ""),
      quote = FALSE, row.names = FALSE
      )

    message("Accepted publication interval")
    o <- as.character(df.orig[["publication_interval"]])
    x <- df.preprocessed[c("publication_interval_from", "publication_interval_till")]
    inds <- which(rowSums(!is.na(x))>0)
    xx <- x[inds,] %>% group_by(publication_interval_from, publication_interval_till) %>% tally() %>% arrange(desc(n))
    tmp <- write.table(xx,
      file = paste(output.folder, "publication_interval_accepted.csv", sep = ""),
      quote = FALSE, row.names = FALSE      
      )

  }

  # --------------------------------------------

  message("Authors with missing life years")
  tab <- df.preprocessed %>% filter(!is.na(author_name) & (is.na(author_birth) | is.na(author_death))) %>% select(author_name, author_birth, author_death)
  tmp <- write.table(tab, file = paste(output.folder, "authors_missing_lifeyears.csv", sep = ""), quote = F, row.names = F)
 
  message("Ambiguous authors with many birth years")
  births <- split(df.preprocessed$author_birth, df.preprocessed$author_name)
  births <- births[sapply(births, length, USE.NAMES = FALSE) > 0]
  many.births <- lapply(births[names(which(sapply(births, function (x) {length(unique(na.omit(x)))}, USE.NAMES = FALSE) > 1))], function (x) {sort(unique(na.omit(x)))})
  dfs <- df.preprocessed[df.preprocessed$author_name %in% names(many.births),
      	 			c("author_name", "author_birth", "author_death")]
  dfs <- unique(dfs)
  dfs <- dfs %>% arrange(author_name, author_birth, author_death)
  write.table(dfs, paste(output.folder, "author_life_ambiguous.csv", sep = ""), quote = F, sep = "\t", row.names = FALSE)

  # -------------------------------------------------------

  message("Undefined language")
  gc(); rm(dfs); rm(tmp) # Cleanup
  # Remove "und" from the list ("Undetermined")
  f <- system.file("extdata/language_abbreviations.csv", package = "bibliographica")
  abrv <- read_mapping(f, include.lowercase = T, self.match = T, ignore.empty = FALSE, mode = "table", sep = "\t")
  # List unique languages that occur in the data
  lang <- tolower(df.orig$language)
  lang <- unique(lang[!is.na(lang)])
  lang <- unlist(strsplit(lang, ";"), use.names = FALSE)
  lang <- unique(lang)
  lang <- lang[!grepl("^[0-9]$", lang)] # Remove numerics
  # Remove the known ones (und is Undetermined)
  known.abbreviations <- setdiff(abrv$synonyme, "und") # und = Undetermined
  discarded.lang <- c("*", ".", "^,", "", "-", "\\\\?", "&")
  unknown.lang <- lang[!lang %in% c(known.abbreviations, discarded.lang)]

  message("Write unknown languages")
  if (length(unknown.lang)>0) {
    ltab <- table(df.orig$language)
    #spl <- unlist(strsplit(names(ltab), ";"), use.names = FALSE)
    # Count occurrences for each unknown lang
    # TODO should be easy to speed up by considering unique entries only
    # and them summing up their stats
    # Identify hits 0/1
    u <- sapply(unknown.lang, function (ul) grepl(paste("^", ul, "$", sep = ""), names(ltab))) |
      	 sapply(unknown.lang, function (ul) grepl(paste("^", ul, ";", sep = ""), names(ltab))) |
      	 sapply(unknown.lang, function (ul) grepl(paste(";", ul, ";", sep = ""), names(ltab))) |
      	 sapply(unknown.lang, function (ul) grepl(paste(";", ul, "$", sep = ""), names(ltab)))

    # Multiply by counts of each case 
    u <- apply(u, 2, function (x) {x * ltab})	 

    # Sum up the occurrence counts for each unknown language
    u <- colSums(u)
    u <- u[u > 0]    
    u <- rev(sort(u))
    tab <- cbind(term = names(u), n = unname(u))
    tmp <- write.csv(tab,
	     file = paste(output.folder, "language_discarded.csv", sep = ""),
	     quote = F, row.names = F)
  } else {
    write.csv("No entries.", file = paste(output.folder, "language_discarded.csv", sep = ""))
  }

  message("Accepted languages")
  known.lang <- lang[lang %in% known.abbreviations]
  tmp <- write_xtable(map(known.lang, abrv), paste(output.folder, "language_accepted.csv", sep = ""), count = TRUE)

  message("Language conversions")
  field = "language"
  original <- as.character(df.orig[[field]])
  polished <- as.character(df.preprocessed[[field]])
  tab <- cbind(original = original, polished = polished)
  tmp <- write_xtable(tab, paste(output.folder, field, "_conversions.csv", sep = ""), count = TRUE)

  # ---------------------------------------------------------

  message("Page counts")
  use.fields <- intersect(c("pagecount", "volnumber", "volcount"), names(df.preprocessed))
  tab <- cbind(original = df.orig$physical_extent, df.preprocessed[, use.fields])
  # For clarity: remove ECCO and Manually augmented pagecounts from ESTC data
  if ("pagecount_from" %in% names(df) & nrow(df.preprocessed) == nrow(df.orig)) {
    tab <- tab[df.preprocessed$pagecount_from %in% c("estc"),]
  }
  tmp <- write.table(tab, file = "output.tables/conversions_physical_extent.csv", quote = F, row.names = F)

  message("Physical dimension info")
  tab <- cbind(original = df.orig$physical_dimension,
               df.preprocessed[, c("gatherings.original", "width.original", "height.original", "obl.original", "gatherings", "width", "height", "obl", "area")])
  tmp <- write.table(tab, file = "output.tables/conversions_physical_dimension.csv", quote = F, row.names = F)

  message("Accepted / Discarded dimension info")
  inds <- which(is.na(df.preprocessed[["area"]]))
  xx <-
    data.frame(original = as.character(df.orig$physical_dimension)[inds],
          df.preprocessed[inds, c("gatherings", "width", "height", "obl")])
  xx <- xx %>% group_by(original, gatherings, width, height, obl) %>% tally() %>% arrange(desc(n))

  tmp <- write.table(xx,
    file = paste(output.folder, paste("physical_dimension_incomplete.csv", sep = "_"), sep = ""),
    quote = F, row.names = F)

  #-----------------------------------------------------------------------

  message("All summary tables generated.")
  gc()

  return(NULL)
}

