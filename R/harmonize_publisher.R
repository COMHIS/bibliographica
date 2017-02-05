#' @title Harmonize Publisher
#' @description Harmonizes publishers with slight misspellings.
#' @param x A vector of publisher names
#' @param publication_year Data frame with "from" and "till" years
#' @param languages A vector of languages which are used in detecting relation keywords
#' @return Data frame with orig, mod and comp 
#' @export
#' @importFrom stringdist amatch
#' @importFrom stringr str_replace
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @examples # harmonize_publisher(x, publication_year, languages=c("finnish", "swedish", "latin"))
#' @keywords utilities
harmonize_publisher <- function(x, publication_year, languages=c("english")) {
  
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
  
  q <- clean_publisher_destructively(q, languages)
  orig_destructed_mapping <- data.frame(orig=r, destructed=q, stringsAsFactors = FALSE)
  message("...orig_destructed_mapping bound")
  # Get the minimum & maximum years for each publisher name
  ranges <- data.frame(min=integer(length(unique(q))), max=integer(length(unique(q))), publisher=character(length(unique(q))), stringsAsFactors=FALSE)
  
  i = 1
  maxdiff = 5
  
  idx <- which(!is.na(publication_year$publication_year_from))
  publication_year$publication_year_from[idx] <- publication_year$publication_year_from[idx]
  publication_year$publication_year_till[idx] <- publication_year$publication_year_from[idx]
  
  # TODO vectorization of for loops with sapply could speed up considerably
  for (pub in unique(q)) {
    
    min_year <- min(publication_year$publication_year_from[(which(q == pub))], na.rm = TRUE)
    max_year <- max(publication_year$publication_year_till[(which(q == pub))], na.rm = TRUE)
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
  x <- cbind.data.frame(publisher=q)
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
  # TODO should not be finland specific !
  compPublisher <- harmonize_for_comparison(na.omit(tab[,1]), languages="finnish")
  # HR 2016-11-08:
  # harmonize_for_comparison added for the whole catalog; this is needed for matching with correct indices
  harmonized_q <- harmonize_for_comparison(q, languages="finnish")
  
  # Initiate & preallocate a new version of publishers
  framePublishers <- data.frame(orig=character(length(id)), mod=character(length(id)), comp=character(length(id)), total=numeric(length(id)), stringsAsFactors = FALSE)
  
  # extract initials etc., in case publisher is a person (may contain lots of false persons)
  # TODO should not be Finland specific !
  unique_personal_names <- extract_personal_names(compPublisher, languages=c("finnish", "latin", "swedish"))
  
  # HR 2016-11-07:
  # Map back unique_personal_names to non-unique, so indices later would match
  personal_names <- unique_personal_names[match(harmonized_q, unique_personal_names$orig),]
  rownames(personal_names) <- NULL
  
  # !!!!!!! THIS IS WRONG IN PRINCIPLE !!!!!!!
  language <- "finnish"
  
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
  
  # HR 2016-10-12: Must change $full_name into empty string from NA, as stringr no longer can handle NAs
  unique_personal_names$full_name[which(is.na(unique_personal_names$full_name))] <- ""
  
  for (i in 1:nrow(synonyms)) {
    pattern <- str_replace(synonyms$synonyme[i], "<name>", unique_personal_names$full_name)
    replacement <- str_replace(synonyms$name[i], "<name>", unique_personal_names$full_name)
    repl_idx <- which(replacement != "")
    compPublisher[repl_idx] <- str_replace(compPublisher[repl_idx], pattern[repl_idx], replacement[repl_idx])
  }

  i <- 1
  message("harmonize_publisher: about to start comparisons")
  # TODO vectorization of for loops with sapply could speed up considerably  
  for (publisherName in na.omit(tab[,1])) {
    # id was set at the same time as tab
    publisherName_indices <- which(id==tab$publisher[i])

    publisherTotal <- unname(unlist(subset(tab, tab[,1]==publisherName)[2]))
    compare_version <- as.character(compPublisher[i])
    initials <- unique_personal_names$initials[i]
    familyname <- unique_personal_names$family[i]
    full_name <- unique_personal_names$full_name[i]
    guessed <- unique_personal_names$guessed[i]
    relation <- unique_personal_names$relation[i]
    
    # HR 2016-11-09:
    # "Alf. Bärlund" <- "Alfred Bärlund", so it will match "A. Bärlund"
    if ((length(grep("[[:upper:]][[:lower:]]+[.] [[:upper:]][[:lower:]]+$", publisherName)) > 0 ) 
        && (full_name != "") && (!is.na(full_name))) {
      publisherName <- full_name
      compare_version <- full_name
    }
    
    # Madness starts here. Must include the publication years
    pub_start <- ranges$min[ranges$publisher==publisherName]
    pub_end <- ranges$max[ranges$publisher==publisherName]
    
    tmp_compare_versions<- framePublishers$comp
    # Check against the caveat list
    # eg. Lundgren mustn't be replaced with Lindgren or vice versa
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
      indices <- intersect(grep(familyname, personal_names$family, invert=TRUE), which(personal_names$guessed==TRUE))
      ignore_indices <- intersect(indices, which(personal_names$guessed==TRUE))
      tmp_compare_versions[ignore_indices] <- NA
      # Insert the comparable version with initials instead of the full names
      fix_indices <- intersect(grep(familyname, personal_names$family), which(personal_names$guessed==TRUE))
      fix_indices <- intersect(fix_indices, which(!is.na(tmp_compare_versions)))
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
      ignore_indices <- intersect(grep(familyname, personal_names$family, invert=TRUE), which(personal_names$guessed==FALSE))
      tmp_compare_versions[ignore_indices] <- NA
      # We must manipulate the comparison group with (possibly imaginary) full names 
      # (Otherwise a parallel comparison would be needed: one for initials, one for full names)
      fix_indices <- intersect(grep(familyname, personal_names$family), which(personal_names$guessed==FALSE))
      fix_indices <- intersect(fix_indices, which(!is.na(tmp_compare_versions)))
      tmp_compare_versions[fix_indices] <- gsub(paste(initials, familyname, collapse=" "), full_name, tmp_compare_versions[fix_indices])
    }
    
    # method="jw" ie. Jaro-Winkler. It takes into account also the length of strings.
    # The thresholds of p & maxDist are produced by Stetson-Harrison method
    res <- framePublishers$orig[amatch(compare_version, tmp_compare_versions, method="jw", p=0.05, maxDist=0.06)]
    
    # 2016-10-04: Added yet another step for case insensitive exact matches
    if ((is.null(res)) || (is.na(res)) || (res=="")) {
      res <- framePublishers$orig[grep(paste0("^", tolower(publisherName), "$"), tolower(na.omit(framePublishers$orig)))][1]
    }
    if ((is.null(res)) || (is.na(res)) || (res=="")) {
      # Add new entry
      framePublishers$orig[publisherName_indices] <- publisherName
      framePublishers$mod[publisherName_indices] <- compare_version
      # HR 2016-10-14: removed the limit of 3 name forms 
      framePublishers$comp[publisherName_indices] <- compare_version
      if (!(compare_version %in% orig_destructed_mapping$destructed)) {
        orig_destructed_mapping$destructed[which(orig_destructed_mapping$destructed==publisherName)] <- compare_version
      } else {
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
      # HR 2016-10-14: removed the limit of 3 name forms 
      framePublishers$comp[publisherName_indices] <- compare_version
      # Map res with orig
      if (!(res[1] %in% orig_destructed_mapping$destructed)) {
        orig_destructed_mapping$destructed[which(orig_destructed_mapping$destructed==publisherName)] <- res[1]
      } else {
      }
    }
    i <- i + 1
    if ((i %% 2000) == 0) {
      print(paste0("...harmonize_publisher: ", i, " publisher names done;   ", Sys.time()))
    }
  }

  message("harmonize_publisher: comparisons done")
  framePublishers$orig <- unlist(as.character(unname(sapply(framePublishers$mod, function(destr) 
    as.character(tail(sort(orig_destructed_mapping$orig[which(orig_destructed_mapping$destructed==destr)]),1))))))
  
  message("harmonize_publisher: matching done")
  return (data.frame(list(orig=framePublishers$orig, mod=framePublishers$mod, comp=framePublishers$comp)))
  
}
