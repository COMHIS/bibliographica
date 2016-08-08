#' @title Harmonize Publisher
#' @description Harmonizes publishers with slight misspellings.
#' @param df bibliographic data.frame with the following fields: publication_year, publisher
#' @param languages A vector of languages which are used in detecting relation keywords
#' @return Polished vector.
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @examples \dontrun{harmonize_publisher(x, publication_year,
#'                      languages = c("finnish", "swedish", "latin"))}
#' @keywords utilities
harmonize_publisher <- function(df, languages = c("english")) {

  publisher <- publication_year_from <- publication_year_till <- NULL

  # Only consider unique terms to speed up		
  xorig <- df[, c("publisher", "publication_year_from", "publication_year_till")]
  xorig$match.id <- unname(apply(xorig, 1, function (x) {paste(x, collapse = "-")}))
  x <- xuniq <- unique(xorig)  

  x$publisher <- harmonize_publishers_per_language(x$publisher, languages)
  x$publisher <- clean_publisher(x$publisher)

  # select the first item from the list
  x$publisher <- gsub("^([^;]+);.*$", "\\1", x$publisher)
  x$publisher <- gsub("^([^(]+)[(].*[)]$", "\\1", x$publisher)
  x$publisher <- gsub("^([^[]+)[[].*[]]$", "\\1", x$publisher)
  x$publisher <- gsub("^[(].*[)]([^(]+)$", "\\1", x$publisher)
  x$publisher <- gsub("^[[].*[]]([^[]+)$", "\\1", x$publisher)

  # remove everything in brackets or parentheses after collecting i komm.,
  # distr., exp., för ... -information    
  # remove naughty characters from the rear
  endings=c(" ", "\\(", "\\)", "\\[", "\\]", "\\.", ";", ":", ",", "'")
  x$publisher <- remove_endings(x$publisher, endings=endings, random_order=TRUE)

  # Back to original indices, then unique again;
  # reduces number of unique cases further
  # Back to original indices, then unique again;
  # reduces number of unique cases further
  xorig <- x[match(xorig$match.id, xuniq$match.id),]    
  x <- xuniq <- unique(xorig)

  # Language wise harmonization
  x$publisher <- harmonize_publishers_per_language(x$publisher, languages)
  x$publisher <- clean_publisher(x$publisher)

  # Back to original indices
  xorig <- x[match(xorig$match.id, xuniq$match.id),]    
  x <- xuniq <- unique(xorig)

  # Get the minimum & maximum years for each publisher name
  ranges <- x %>% group_by(publisher) %>%
                  summarise(
		    min = min(publication_year_from, na.rm = TRUE),
		    max = min(publication_year_till, na.rm = TRUE)
		  )

  # Build the stop mechanism
  # NB! Hardcoded for Finnish. Language specific handling required.
  # TODO vectorization of for loops with sapply could speed up considerably
  f <- system.file("extdata/fi_publisher_caveat.csv", package="bibliographica")
  caveats <- read.csv(f, sep = "\t", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  colnames(caveats) <- NULL
  cav1 <- cbind(caveats[, 1], caveats[, 2])
  cav2 <- cbind(caveats[, 2], caveats[, 1])
  cav <- data.frame(rbind(cav1, cav2), stringsAsFactors = FALSE)
  names(cav) <- c("name1", "name2")

  # Get the publisher name forms into a table
  id <- as.character(x$publisher)
  tab <- table(id)

  # prepare_nameforms (remove initials, transform fv -> v; fw -> v ... etc.)
  compPublisher <- harmonize_for_comparison(na.omit(names(tab)),
			languages = languages)

  # Initiate & preallocate a new version of publishers
  framePublishers <- data.frame(orig  = character(length(id)),
  		                mod   = character(length(id)),
				comp  = character(length(id)),
				total = numeric(length(id)),
				stringsAsFactors = FALSE)

  # extract initials etc., in case publisher is a person
  # (may contain lots of false persons)
  personal_names <- extract_personal_names(compPublisher,
  		    languages=c("finnish", "latin", "swedish", "english"))
  
  if ("finnish" %in% languages) {
    f <- system.file("extdata/fi_publisher_with_placeholders.csv",
			package="bibliographica")
  }
  
  synonyms <- read.csv(file = f, sep = "\t", fileEncoding = "UTF-8")
  
  # TODO vectorization of for loops with sapply could speed up considerably  
  for (i in 1:nrow(synonyms)) {
    pattern <- str_replace(synonyms$synonyme[i], "<name>",
    	       					 personal_names$full_name)
    replacement <- str_replace(synonyms$name[i], "<name>",
    		   				 personal_names$full_name)
						 
    repl_idx <- which(!is.na(replacement))
    compPublisher[repl_idx] <- str_replace(compPublisher[repl_idx],
				pattern[repl_idx], replacement[repl_idx])
  }

  i <- 1
  maxdiff <- 5
  maxrange <- 40  
  # TODO vectorization of for loops with sapply could speed up considerably  
  for (publisherName in na.omit(names(tab))) {

        # id was set at the same time as tab
        publisherName_indices <- which(id == publisherName)

        publisherTotal <- tab[[which(names(tab) == publisherName)]]
        compare_version <- as.character(compPublisher[i])

        initials <- personal_names$initials[i]
        familyname <- personal_names$family[i]
        full_name <- personal_names$full_name[i]
        guessed <- personal_names$guessed[i]
        relation <- personal_names$relation[i]
        
        # Madness starts here. Must include the publication years
        pub_start <- ranges$min[ranges$publisher==publisherName]
        pub_end <- ranges$max[ranges$publisher==publisherName]
        
        # SCRAP HERE, if too slow
        tmp_compare_versions <- framePublishers$comp
	
        # Check against the caveat list
        for (j in 1:nrow(cav)) {
          if (length(grep(cav$name1[j], compare_version)) > 0) {
            indices <- grep(cav$name2[j], tmp_compare_versions)
            indices <- intersect(grep(cav$name1[j], tmp_compare_versions, invert=TRUE), indices)
            tmp_compare_versions[indices] <- NA
          }
        }
        
        # Continuing the madness. If the publisher is only related to the
	# person mentioned, it overrides almost anything

        if (!is.na(relation) && relation != "") {
	
          # Ignore everything, where the relations won't match
          ignore_indices <- tmp_compare_versions[which(personal_names$relation!=relation)]
          tmp_compare_versions[ignore_indices] <- NA
          # Create a new comparison version, stripped of anything but
	  # the name and relation
          compare_version <- paste(full_name, relation, sep=" ", collapse="")
        }

        # Now, the initials. In case of initials, they must be the same in
	# tmp_compare_version and the string to match
        # Remove from indices all the indices without the same last
	# name and possibly the same initials
        if ((!is.na(initials)) && (!is.na(guessed)) && (!guessed)) {
          # We're talking about persons, when we have the initials
          # So we'll exclude any invalid publication year ranges from the
	  # comparison group
          ignore_publishers <- ranges$publisher[which(ranges$min > (pub_end + maxdiff))]
          if (length(ignore_publishers) > 0) {
            ignore_indices <- which(framePublishers$comp %in% ignore_publishers)
            tmp_compare_versions[ignore_indices] <- NA
          }
          ignore_publishers <- ranges$publisher[which(ranges$max < (pub_start - maxdiff))]
          if (length(ignore_publishers) > 0) {
            ignore_indices <- which(framePublishers$comp %in% ignore_publishers)
            tmp_compare_versions[ignore_indices] <- NA
          }
          
          # Case: H.H.M. Raivoinen vs. c("H.H.M. Raivoinen", "J.R. Raivoinen")
          # Required: Exactly the same initials (full name unknown)
          ignore_indices <- which(personal_names$initials != initials)
          tmp_compare_versions[ignore_indices] <- NA
          
          # Case: H.H.M Raivoinen vs. c("Hege Henri Markus Raivoinen",
          # "Hege On Tyhmä") Required: exactly the same initials, full
          # name known only in comparison group, exactly the same last
          # name
          indices <- grep(familyname,
	    personal_names$family[which(personal_names$guessed)],
	    invert = TRUE)
          ignore_indices <- intersect(indices,
	    which(personal_names$guessed))
          tmp_compare_versions[ignore_indices] <- NA
	  
          # Insert the comparable version with initials instead of the
          # full names
	  fix_indices <- grep(familyname,
	  	      personal_names$family[personal_names$guessed])

          tmp_compare_versions[fix_indices] <- as.character(personal_names$init_name[fix_indices])

        } else if ((!is.na(initials)) && (!is.na(guessed)) && (guessed)) {
	
          # We're talking about persons, when we have the initials
          # So we'll exclude any invalid publication year ranges
	  # from the comparison group
          ignore_publishers <- ranges$publisher[which(ranges$min > (pub_end + maxdiff))]
          if (length(ignore_publishers) > 0) {
            ignore_indices <- which(framePublishers$comp %in% ignore_publishers)
            tmp_compare_versions[ignore_indices] <- NA
          }
	  
          ignore_publishers <- ranges$publisher[which(ranges$max < (pub_start - maxdiff))]
          if (length(ignore_publishers) > 0) {
            ignore_indices <- which(framePublishers$comp %in% ignore_publishers)
            tmp_compare_versions[ignore_indices] <- NA
          }
          
          # Case Hege Henri Markus Raivoinen vs. c("H.H.M. Raivoinen", "J.R. Raivoinen")
          # Required: exactly the same initials, full name known only in the
	  # comparison string, exactly the same last name
          ignore_indices <- which(personal_names$initials != initials)
          tmp_compare_versions[ignore_indices] <- NA
          ignore_indices <- grep(familyname, personal_names$family[personal_names$guessed==FALSE], invert=TRUE)
          tmp_compare_versions[ignore_indices] <- NA
          # We must manipulate the comparison group with (possibly imaginary) full names 
          # (Otherwise a parallel comparison would be needed: one for initials, one for full names)
          fix_indices <- grep(familyname, personal_names$family[personal_names$guessed==FALSE])
          tmp_compare_versions[fix_indices] <- gsub(paste(initials, familyname, collapse=" "), full_name, tmp_compare_versions[fix_indices])
        }

        # method = "jw" ie. Jaro-Winkler.
	# takes into account also the length of strings.
        # The thresholds of p & maxDist are by
	# Stetson-Harrison method

        res <- framePublishers$orig[amatch(compare_version,
	         tmp_compare_versions,
	       	 method = "jw",
		 p = 0.05, maxDist = 0.06)]

        if (length(res) == 0 ||  is.null(res)  ||
	     is.na(res)  || (res == ""))  {

	  if (length(publisherName_indices) > 0 &&
	      length(compare_version)>0) {
	    
            # Add new entry
            framePublishers$orig[publisherName_indices] <- publisherName
            framePublishers$mod[publisherName_indices]  <- compare_version
	    
            if (publisherTotal > 2) {
              framePublishers$comp[publisherName_indices] <- compare_version
            }
	  }
	  
        } else {

          # Check if the matched res is matched to something else already
          res <- framePublishers$mod[framePublishers$orig == res]

          # Add combined total for the publisher name already found
          combinedTotal <- (unname(unlist(framePublishers$total[which(framePublishers$orig==res)])) +
	                          publisherTotal)
          framePublishers$total[framePublishers$orig == res] <- combinedTotal
          
          # Update the year range for the publisher names already found
          involved_publisher_names <- framePublishers$orig[framePublishers$mod==res]
          new_start_year <- min(ranges$min[which(ranges$publisher %in% involved_publisher_names)], pub_start)
          new_end_year <- max(ranges$max[which(ranges$publisher %in% involved_publisher_names)], pub_end)
          ranges$min[which(ranges$publisher %in% involved_publisher_names)] <- new_start_year
          ranges$max[which(ranges$publisher %in% involved_publisher_names)] <- new_end_year
          
          # Add new entry for the modified result
	  if (length(publisherName_indices) > 0) {

            framePublishers$orig[publisherName_indices] <- publisherName
            framePublishers$mod[publisherName_indices]  <- res
            if (publisherTotal > 2) {
              framePublishers$comp[publisherName_indices] <- compare_version
	    }
          }
        }
        
        i <- i + 1
        
  }

  # Back to original indices & Speed up things by only returning mod
  pub <- framePublishers$mod[match(xorig$match.id, xuniq$match.id)]
  pub <- str_trim(pub)

  return(pub)  
  
}
