combine_publisher_fennica <- function(df.orig, languages=c("english"), enriched_pubs, town, publication_year, cheat_list) {

  # Prepare new publisher variable, for those that haven't been processed yet
  enriched_inds <- which(enriched_pubs$alt != "")
  enriched_inds <- intersect(which(!is.na(enriched_pubs$alt)), enriched_inds)
  
  # TODO: Shortcut: exit if empty
  
  unprocessed_pubs <- clean_publisher(df.orig$publisher[-enriched_inds], languages=languages)
  unprocessed_pubs <- harmonize_publisher(unprocessed_pubs, publication_year[-enriched_inds], languages=languages)
  #unprocessed_towns <- df.orig$publication_place[-enriched_inds]
  unprocessed_towns <- df.orig$publication_place[-enriched_inds]
  unprocessed_years <- publication_year[-enriched_inds,]
  
  # Bugs fixed: Remove opening brackets without closure, as grep considers that naughty
  unprocessed_towns <- gsub("^[[]([^[]+)$","\\1", unprocessed_towns)
  unprocessed_towns <- gsub("^[[](.*[^]])$","\\1", unprocessed_towns)
  preferred_pubs <- change_to_Finto_preferred(pubs=unprocessed_pubs, towns=unprocessed_towns, years=unprocessed_years, cheat_list=cheat_list)
  
  # Combine the two sets of the combined publishers
  combined_pubs <- data.frame(orig=character(length=nrow(enriched_pubs)), mod=character(length=nrow(enriched_pubs)), method=character(length=nrow(enriched_pubs)), stringsAsFactors = FALSE)
  combined_pubs$orig[enriched_inds] <- enriched_pubs$alt[enriched_inds]
  combined_pubs$mod[enriched_inds] <- enriched_pubs$pref[enriched_inds]
  #combined_pubs$method[enriched_inds] <- 4
  
  #combined_pubs$method[-enriched_inds] <- enriched_pubs$match_method[-enriched_inds]
  combined_pubs$orig[-enriched_inds] <- preferred_pubs$orig
  combined_pubs$mod[-enriched_inds] <- preferred_pubs$mod

  return (combined_pubs)
}