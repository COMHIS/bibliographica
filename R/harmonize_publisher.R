#' @title Harmonize Publisher
#' @description Harmonizes publishers with slight misspellings.
#' @param x A vector of publisher names
#' @param publication_year Data frame with "from" and "till" years
#' @param languages A vector of languages which are used in detecting relation keywords
#' @return Polished vector.
#' @export
#' @importFrom stringdist amatch
#' @importFrom stringr str_replace
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @examples # harmonize_publisher(x, publication_year, languages=c("finnish", "swedish", "latin"))
#' @keywords utilities
harmonize_publisher <- function(x, publication_year, languages=c("english")) {

  # Clean up first		    
  x <- clean_publisher(x, languages = languages)

  # Only consider unique terms to speed up		
  xorig <- as.character(x)
  x <- xuniq <- unique(xorig)  

  q <- x
  
  # select the first item from the list
  q <- gsub("^([^;]+);.*$", "\\1", q)
  q <- gsub("^([^(]+)[(].*[)]$", "\\1", q)
  q <- gsub("^([^[]+)[[].*[]]$", "\\1", q)
  q <- gsub("^[(].*[)]([^(]+)$", "\\1", q)
  q <- gsub("^[[].*[]]([^[]+)$", "\\1", q)
  
  # remove everything in brackets or parentheses after collecting i komm., distr., exp., för ... -information
  # TBD
    
  # remove naughty characters from the rear
  endings=c(" ", "\\(", "\\)", "\\[", "\\]", "\\.", ";", ":", ",", "'")
  q <- remove_endings(q, endings=endings, random_order=TRUE)

  # Back to original indices, then unique again;
  # reduces number of unique cases further
  # Back to original indices, then unique again;
  # reduces number of unique cases further
  xorig <- q[match(xorig, xuniq)]
  q <- xuniq <- unique(xorig)

  for (language in languages) {
    if (language=="swedish") {
      f <- system.file("extdata/sv_publisher.csv",package = "bibliographica")
    } else if (language=="english") {
      
    } else if (language=="finnish") {
      f <- system.file("extdata/fi_publisher.csv", package = "bibliographica")  
    }
    synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")
    q <- map(q, synonyms, mode="recursive")
  }
  
  

  # remove brackets and parentheses (Destructive phase)
  q <- gsub("^[(](.*)[)]$", "\\1", q)
  q <- gsub("^[[](.*)[]]$", "\\1", q)
  q <- gsub("[[]", "", q)
  q <- gsub("[]]", "", q)
  q <- gsub("[(]", "", q)
  q <- gsub("[)]", "", q)
  
  # add space when needed
  q <- gsub("([[:upper:]])&", "\\1 &", q)
  q <- gsub("&([[:upper:]])", "& \\1", q)
  q <- gsub("([[:lower:]])&", "\\1 &", q)
  q <- gsub("&([[:lower:]])", "& \\1", q)

  # harmonize initials
  # CWK Raivoinen -> C.W.K. Raivoinen; C. W. K. Raivoinen -> C.W.K. Raivoinen
  q <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1.\\2.\\3. \\4", q)
  q <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1.\\2. \\3", q)
  q <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1. \\2", q)

  # Back to original indices
  q <- q[match(xorig, xuniq)]

  # Backup the original form at this stage
  r <- q
  
  # Get the minimum & maximum years for each publisher name
  ranges <- data.frame(min=integer(length(unique(q))), max=integer(length(unique(q))), publisher=character(length(unique(q))), stringsAsFactors=FALSE)
  
  i = 1
  maxdiff = 5
  maxrange = 40
  
  idx <- which(!is.na(publication_year$from))
  publication_year$from[idx] <- publication_year$from[idx]
  publication_year$till[idx] <- publication_year$from[idx]

  # TODO vectorization of for loops with sapply could speed up considerably
  for (pub in unique(q)) {
    
    min_year <- min(publication_year$from[(which(q == pub))], na.rm = TRUE)
    max_year <- max(publication_year$till[(which(q == pub))], na.rm = TRUE)
    if (is.na(pub)) {
      next
    }
    if ((!is.infinite(min_year)) && (is.infinite(max_year))) {
      max_year <- min_year
    } else if ((!is.infinite(max_year)) && (is.infinite(min_year))) {
      min_year <- max_year
    } else if ((is.infinite(min_year)) && (is.infinite(max_year))) {
        ranges$min[i] <- ranges$max[i] <- NA
        ranges$publisher[i] <- pub
        i = i + 1
        next
    }
    ranges$min[i] <- min_year
    ranges$max[i] <- max_year
    ranges$publisher[i] <- pub
    i = i + 1
    
  }
  
  # Get the publisher name forms into a table
  x <- cbind.data.frame(publisher=r)
  id <- apply(x, 1, function (x) {paste(x, collapse = "-")})
  ido <- rev(sort(table(id)))
  idn <- ido[match(id, names(ido))]
  tab <- cbind(x, count = as.vector(idn))
  tab <- tab[rev(order(as.numeric(tab[, "count"]))),]
  tab <- tab[!duplicated(tab),]
  tab <- na.omit(tab)
  
  total <- sum(tab[,2])

  # Build the stop mechanism
  # NB! Hardcoded for Finnish. Language specific handling required.
  # TODO vectorization of for loops with sapply could speed up considerably
  f <- system.file("extdata/fi_publisher_caveat.csv", package="bibliographica")
  caveats <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")
  cav <- data.frame(name1=character(nrow(caveats)*2), name2=character(nrow(caveats)*2), stringsAsFactors=FALSE)
  for (i in 1:nrow(caveats)) {
    cav$name1[i] <- as.character(caveats$name1[i])
    cav$name2[i] <- as.character(caveats$name2[i])
    cav$name1[i+nrow(caveats)] <- as.character(caveats$name2[i])
    cav$name2[i+nrow(caveats)] <- as.character(caveats$name1[i])
  }
  
  # prepare_nameforms (remove initials, transform fv -> v; fw -> v ... etc.)
  compPublisher <- harmonize_for_comparison(na.omit(tab[,1]), language="finnish")

  # Initiate & preallocate a new version of publishers
  #framePublishers <- data.frame(orig=character(nrow(tab)), mod=character(nrow(tab)), comp=character(nrow(tab)), total=numeric(nrow(tab)), stringsAsFactors = FALSE)
  framePublishers <- data.frame(orig=character(length(id)), mod=character(length(id)), comp=character(length(id)), total=numeric(length(id)), stringsAsFactors = FALSE)
  
  # extract initials etc., in case publisher is a person (may contain lots of false persons)
  personal_names <- extract_personal_names(compPublisher, languages=c("finnish", "latin", "swedish"))
  
  if (language=="finnish") {
    f = system.file("extdata/fi_publisher_with_placeholders.csv", package="bibliographica")
  } else if (language=="swedish") {
    # TODO    
  } else if (language=="english") {
    # TODO
  } else {
    
  }
  synonyms <- read.csv(file=f, sep="\t", fileEncoding="UTF-8")
  # TODO vectorization of for loops with sapply could speed up considerably  
  for (i in 1:nrow(synonyms)) {
    pattern <- str_replace(synonyms$synonyme[i], "<name>", personal_names$full_name)
    replacement <- str_replace(synonyms$name[i], "<name>", personal_names$full_name)
    repl_idx <- which(!is.na(replacement))
    compPublisher[repl_idx] <-str_replace(compPublisher[repl_idx], pattern[repl_idx], replacement[repl_idx])
  }
  
  i <- 1
  # TODO vectorization of for loops with sapply could speed up considerably  
  for (publisherName in na.omit(tab[,1])) {
    
        # id was set at the same time as tab
        publisherName_indices <- which(id==tab$publisher[i])
        
        publisherTotal <- unname(unlist(subset(tab, tab[,1]==publisherName)[2]))
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
        tmp_compare_versions<- framePublishers$comp
        # Check against the caveat list
        for (j in 1:nrow(cav)) {
          if (length(grep(cav$name1[j], compare_version)) > 0) {
            indices <- grep(cav$name2[j], tmp_compare_versions)
            indices <- intersect(grep(cav$name1[j], tmp_compare_versions, invert=TRUE), indices)
            tmp_compare_versions[indices] <- NA
          }
        }
        
        # Continuing the madness. If the publisher is only related to the person mentioned, it overrides almost anything
        if (relation != "") {
          # Ignore everything, where the relations won't match
          ignore_indices <- tmp_compare_versions[which(personal_names$relation!=relation)]
          tmp_compare_versions[ignore_indices] <- NA
          # Create a new comparison version, stripped of anything but the name and relation
          compare_version <- paste(full_name, relation, sep=" ", collapse="")
        }
        # Now, the initials. In case of initials, they must be the same in tmp_compare_version and the string to match
        # Remove from indices all the indices without the same last name and possibly the same initials
        if ((!is.na(initials)) && (!is.na(guessed)) && (guessed==FALSE)) {
          # We're talking about persons, when we have the initials
          # So we'll exclude any invalid publication year ranges from the comparison group
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
          
          # Case: H.H.M Raivoinen vs. c("Hege Henri Markus Raivoinen", "Hege On Tyhmä")
          # Required: exactly the same initials, full name known only in comparison group, exactly the same last name
          indices <- grep(familyname, personal_names$family[which(personal_names$guessed==TRUE)], invert=TRUE)
          ignore_indices <- intersect(indices, which(personal_names$guessed==TRUE))
          tmp_compare_versions[ignore_indices] <- NA
          # Insert the comparable version with initials instead of the full names
          fix_indices <- grep(familyname, personal_names$family[personal_names$guessed==TRUE])

          tmp_compare_versions[fix_indices] <- as.character(personal_names$init_name[fix_indices])

        } else if ((!is.na(initials)) && (!is.na(guessed)) && (guessed==TRUE)) {
          # We're talking about persons, when we have the initials
          # So we'll exclude any invalid publication year ranges from the comparison group
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
          # Required: exactly the same initials, full name known only in the comparison string, exactly the same last name
          ignore_indices <- which(personal_names$initials != initials)
          tmp_compare_versions[ignore_indices] <- NA
          ignore_indices <- grep(familyname, personal_names$family[personal_names$guessed==FALSE], invert=TRUE)
          tmp_compare_versions[ignore_indices] <- NA
          # We must manipulate the comparison group with (possibly imaginary) full names 
          # (Otherwise a parallel comparison would be needed: one for initials, one for full names)
          fix_indices <- grep(familyname, personal_names$family[personal_names$guessed==FALSE])
          tmp_compare_versions[fix_indices] <- gsub(paste(initials, familyname, collapse=" "), full_name, tmp_compare_versions[fix_indices])
        }
        
        # method="jw" ie. Jaro-Winkler. It takes into account also the length of strings.
        # The thresholds of p & maxDist are produced by Stetson-Harrison method
        res <- framePublishers$orig[amatch(compare_version, tmp_compare_versions, method="jw", p=0.05, maxDist=0.06)]
        
        if ((is.null(res)) || (is.na(res)) || (res=="")) {
          # Add new entry
          framePublishers$orig[publisherName_indices] <- publisherName
          framePublishers$mod[publisherName_indices] <- compare_version
          if (publisherTotal > 2) {
            framePublishers$comp[publisherName_indices] <- compare_version
          }
        } else {
          # Check if the matched res is matched to something else already
          res <- framePublishers$mod[framePublishers$orig==res]
          
          # Add combined total for the publisher name already found
          combinedTotal <- (unname(unlist(framePublishers$total[which(framePublishers$orig==res)])) + publisherTotal)
          framePublishers$total[framePublishers$orig==res] <- combinedTotal
          
          # Update the year range for the publisher names already found
          involved_publisher_names <- framePublishers$orig[framePublishers$mod==res]
          new_start_year <- min(ranges$min[which(ranges$publisher %in% involved_publisher_names)], pub_start)
          new_end_year <- max(ranges$max[which(ranges$publisher %in% involved_publisher_names)], pub_end)
          ranges$min[which(ranges$publisher %in% involved_publisher_names)] <- new_start_year
          ranges$max[which(ranges$publisher %in% involved_publisher_names)] <- new_end_year
          
          # Add new entry for the modified result
          framePublishers$orig[publisherName_indices] <- publisherName
          framePublishers$mod[publisherName_indices] <- res
          if (publisherTotal > 2) {
            framePublishers$comp[publisherName_indices] <- compare_version
          }
        }
        
        i <- i + 1
        
  }

  # Speed up things by only returning mod
  #return (data.frame(list(orig=framePublishers$orig, mod=framePublishers$mod, comp=framePublishers$comp)))
  return(framePublishers$mod)  
  
}
