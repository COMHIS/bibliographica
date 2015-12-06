#' @importFrom sorvi harmonize_names
#' 
harmonize_publisher <- function(x, publication_year, language="english") {
  #f <- system.file("extdata/sv_publisher.csv", package = "bibliographica")
  f <- "../inst/extdata/sv_publisher.csv"
  synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")
  #synonyms$synonyme <- paste(synonyms$synonyme, "[ \\b] ",sep = "")
  
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
  
  q <- harmonize_names(q, synonyms, mode="recursive")$name

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
  
  # Backup the original form at this stage
  r <- q
  
  # Get the minimum & maximum years for each publisher name
  ranges <- data.frame(min=integer(length(unique(q))), max=integer(length(unique(q))), publisher=character(length(unique(q))), stringsAsFactors=FALSE)
  
  
  # extra round to harmonize the ortography through times
  #f <- system.file("extdata/sv_publisher_comparison.csv", package = "bibliographica")
  #f <- "../inst/extdata/sv_publisher_comparison.csv"
  #synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")
  #q <- harmonize_names(q, synonyms, mode="recursive")$name
  
  i = 1
  maxdiff = 5
  maxrange = 40
  
  idx <- which(!is.na(publication_year$published_in))
  publication_year$published_from[idx] <- publication_year$published_in[idx]
  publication_year$published_till[idx] <- publication_year$published_in[idx]
  
  for (pub in unique(q)) {
    
    min_year <- min(publication_year$published_from[(which(q==pub))], na.rm=TRUE)
    max_year <- max(publication_year$published_till[(which(q==pub))], na.rm=TRUE)
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
  tab <- cbind(x, count = idn)
  tab <- tab[rev(order(as.numeric(tab[, "count"]))),]
  tab <- tab[!duplicated(tab),]
  
  total <- sum(tab[,2])

  # Build the stop mechanism
  f <- "../inst/extdata/sv_publisher_caveat.csv"
  caveats <- read.csv(f, sep = "\t", fileEncoding = "UTF-8",)
  cav <- data.frame(name1=character(nrow(caveats)*2), name2=character(nrow(caveats)*2), stringsAsFactors=FALSE)
  for (i in 1:nrow(caveats)) {
    cav$name1[i] <- as.character(caveats$name1[i])
    cav$name2[i] <- as.character(caveats$name2[i])
    cav$name1[i+nrow(caveats)] <- as.character(caveats$name2[i])
    cav$name2[i+nrow(caveats)] <- as.character(caveats$name1[i])
  }
  
  # prepare_nameforms (remove initials, transform fv -> v; fw -> v ... etc.)
  compPublisher <- harmonize_for_comparison(na.omit(tab[,1]), language="swedish")

  # Initiate & preallocate a new version of publishers
  framePublishers <- data.frame(orig=character(nrow(tab)), mod=character(nrow(tab)), comp=character(nrow(tab)), total=numeric(nrow(tab)), stringsAsFactors = FALSE)
  
  # extract initials etc., in case publisher is a person (may contain lots of false persons)
  personal_names <- extract_personal_names(compPublisher)
  
  i <- 1
  for (publisherName in na.omit(tab[,1])) {
        publisherTotal <- unname(unlist(subset(tab, tab[,1]==publisherName)[2]))
        compare_version <- as.character(compPublisher[i])
        initials <- personal_names$initials[i]
        familyname <- personal_names$family[i]
        full_name <- personal_names$full_name[i]
        guessed <- personal_names$guessed[i]
        
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
        
        # Now, the initials. In case of initials, they must be the same in tmp_compare_version and the string to match
        # Remove from indices all the indices without the same last name and possibly the same initials
        if ((!is.na(initials)) && (guessed==FALSE)) {
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
          
        } else if ((!is.na(initials)) && (guessed==TRUE)) {
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
          framePublishers$orig[i] <- publisherName
          framePublishers$mod[i] <- compare_version
          framePublishers$total[i] <- publisherTotal
          if (publisherTotal > 2) {
            framePublishers$comp[i] <- compare_version
          }
        } else {
          
          # Check if the matched res is matched to something else already
          res <- framePublishers$mod[framePublishers$orig==res]
          
          # Add combined total for the publisher name already found
          combinedTotal = (unname(unlist(framePublishers$total[which(framePublishers$orig==res)])) + publisherTotal)
          framePublishers$total[framePublishers$orig==res] <- combinedTotal
          
          # Update the year range for the publisher names already found
          involved_publisher_names <- framePublishers$orig[framePublishers$mod==res]
          new_start_year <- min(ranges$min[which(ranges$publisher %in% involved_publisher_names)], pub_start)
          new_end_year <- max(ranges$max[which(ranges$publisher %in% involved_publisher_names)], pub_end)
          ranges$min[which(ranges$publisher %in% involved_publisher_names)] <- new_start_year
          ranges$max[which(ranges$publisher %in% involved_publisher_names)] <- new_end_year
          
          # Add new entry for the modified result
          framePublishers$orig[i] <- publisherName
          framePublishers$mod[i] <- res
          framePublishers$total[i] <- 0
          if (publisherTotal > 2) {
            framePublishers$comp[i] <- compare_version
          }
        }
        
        i <- i + 1
        
  }
  #print(length(framePublishers$orig))
  data.frame(list(orig=framePublishers$orig, mod=framePublishers$mod, comp=framePublishers$comp, total=framePublishers$total))
  
  #framePublishers[which(orig!=mod),][1:100,]
  
  #length(unique(orig))
  #length(unique(mod))
  
}