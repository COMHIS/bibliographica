#' @importFrom sorvi harmonize_names
#' 

  #f <- system.file("extdata/translation_sv_en_publisher.csv", package = "bibliographica")
  f <- "../inst/extdata/sv_publisher.csv"
  synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")
  #synonyms$synonyme <- paste(synonyms$synonyme, "[ \\b] ",sep = "")
  
  q <- x <- df.orig$publisher
  
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
  
  q <- harmonize_names(x, synonyms, mode="recursive")$name
  r <- q
  
  # remove brackets and parentheses
  q <- gsub("^[(](.*)[)]$", "\\1", q)
  q <- gsub("^[[](.*)[]]$", "\\1", q)
  
  length(unique(x))
  length(unique(r))
  length(unique(q))
  
  
  
  # risky operation, remove (hopefully) possessive suffix from the end
  #q <- remove_endings(q, endings=" i")
  #q <- remove_endings(q, endings="s")
  #q <- remove_endings(q, endings=c("en", "et", "n", "na"))
  
  
  # harmonize initials
  # CWK Raivoinen -> C.W.K. Raivoinen; C. W. K. Raivoinen -> C.W.K. Raivoinen
  q <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1.\\2.\\3. \\4", q)
  q <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1.\\2. \\3", q)
  q <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1. \\2", q)
  
  # Remove punctuation marks from the ends of the words?
  #q <- gsub("([[:lower:]])([.?,])+([.?,])?\\b", "\\1", q)
  #q <- harmonize_names(q, synonyms, mode="recursive")$name
  #length(unique(q))
  
  # Get the publisher name forms, which belong to the most common 90%, or at least 50 books
  x <- cbind.data.frame(publisher=q)
  id <- apply(x, 1, function (x) {paste(x, collapse = "-")})
  ido <- rev(sort(table(id)))
  idn <- ido[match(id, names(ido))]
  tab <- cbind(x, count = idn)
  tab <- tab[rev(order(as.numeric(tab[, "count"]))),]
  tab <- tab[!duplicated(tab),]
  
  total <- sum(tab[,2])
  #cum <- cumsum(tab[,2])
  #top_ninety <- .90 * total 
  #top_ninety_publishers <- tab[which(cum < top_ninety),]
  #max <- unname(top_ninety_publishers[nrow(top_ninety_publishers),2])
  #top_ninety_publishers <- tab[which(as.numeric(tab[,2]) >= max),]
  #bottom_ten_publishers <- tab[which(as.numeric(tab[,2]) < max),]
  
  # prepare_nameforms (remove initials, transform fv -> v; fw -> v ... etc.)
  compPublisher <- harmonize_for_comparison(na.omit(tab[,1]), language="swedish")
  
  # adist(90percent_nameforms, all_nameforms)
  
  # Initiate & preallocate a new version of publishers
  framePublishers <- data.frame(orig=character(nrow(tab)), mod=character(nrow(tab)), comp=character(nrow(tab)), total=numeric(nrow(tab)), stringsAsFactors = FALSE)
  
  
  i <- 1
  for (publisherName in na.omit(tab[,1])) {
        publisherTotal <- unname(unlist(subset(tab, tab[,1]==publisherName)[2]))
        compare_version <- as.character(compPublisher[i])
        
        #res <- framePublishers$orig[amatch(compare_version, framePublishers$comp, maxDist=1)]
        
        # method="jw" ie. Jaro-Winkler. It takes into account also the length of strings.
        res <- framePublishers$orig[amatch(compare_version, framePublishers$comp, method="jw", p=0.05, maxDist=0.08)]
        
        if ((is.null(res)) || (is.na(res)) || (res=="")) {
          # Add new entry
          framePublishers$orig[i] <- publisherName
          framePublishers$mod[i] <- publisherName
          #framePublishers$comp[i] <- compare_version
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

          # Add new entry for the modified result
          framePublishers$orig[i] <- publisherName
          framePublishers$mod[i] <- res
          #framePublishers$comp[i] <- compare_version
          framePublishers$total[i] <- 0
          if (publisherTotal > 2) {
            framePublishers$comp[i] <- compare_version
          }
        }
        i <- i + 1
        
  }
  orig <- framePublishers$orig
  mod <- framePublishers$mod
  framePublishers[which(orig!=mod),][1:100,]
  # ignore 3+ results
  
  
  
  
  