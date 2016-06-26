harmonize_publisher_main <- function (datasource, df.orig, testing_max=NULL) {

  # TODO : one way to speed up is to only consider unique entries. 

  enrich <- FALSE

  # TODO: Get necessary function names, tables etc. from a single csv-file!
  additional_harmonizing_function <- NA
  
  if (datasource == "fennica") {
    languages <- c("finnish", "latin", "swedish")
    enrich <- TRUE
    enrichment_function <- "harmonize_publisher_fennica"
    additional_harmonizing_function <- "harmonize_corporate_Finto"
    combining_function <- "combine_publisher_fennica"

    # FIXME : polish_years should be replaced with the newer
    # and more generic function polish_years whenever time allows
    
    publication_year <- polish_years(df.orig$publication_time)
    town <- polish_place(df.orig$publication_place)
    cheat_list <- cheat_publishers()
    inds <- which(!is.na(df.orig$corporate))
    
    
  } else if (datasource == "kungliga") {
    languages <- c("swedish")
    enrich <- FALSE
    inds <- integer(length(0))
    publication_year <- polish_years(df.orig$publication_time)
    raw_publishers <- df.orig$publisher
    raw_publishers[which(is.na(raw_publishers))] <- df.orig$corporate[which(is.na(raw_publishers))]
  }
  
  # FOR TESTING PURPOSES ONLY
  # THE WHOLE PROCESS IS SLOW, SO TESTING WITH PARTIAL MATERIAL IS NECESSARY
  if (!is.null(testing_max)) {
    df.orig <- df.orig[1:testing_max,]
    inds <- intersect(1:testing_max, inds)
    publication_year <- publication_year[1:testing_max,]
  }
  df <- data.frame(list(row.index = 1:nrow(df.orig)))
  
  # Initiate pubs
  pubs <- data.frame(alt=character(length=nrow(df.orig)), pref=character(length=nrow(df.orig)), match_method=integer(length=nrow(df.orig)), stringsAsFactors = FALSE)
  
  # Additional harmonizing: in Fennica there's stuff in $corporate -field, which doesn't match with Finto
  # TODO: Would be very good to separate the catalog specific parts outside of bibliographica
  if (!is.na(additional_harmonizing_function)) {
    additionally_harmonized <- do.call(additional_harmonizing_function, list(df.orig$corporate[inds]))
    pubs$alt[inds] <- additionally_harmonized$orig
    pubs$pref[inds] <- additionally_harmonized$name
    pubs$match_method[inds] <- 4
  }
  
  # The enrichment part
  # TODO: enrichments should be in a separate function for clarity, as with the other fields in the pipeline.
  # But this is ok an very useful for now  
  if (enrich) {
    enriched_pubs <- do.call(enrichment_function, args=list(df.orig, cheat_list=cheat_list, languages=languages))
  } else {
    enriched_pubs <- data.frame(alt=character(length=0), pref=character(length=0), match_methods=character(length=0), stringsAsFactors=FALSE)
  }
  
  enriched_inds <- which(enriched_pubs$alt!="")
  #processed_inds <- union(inds, enriched_inds)
  
  # Check if this is valid
  pubs$alt[enriched_inds] <- enriched_pubs$alt[enriched_inds]
  
  # CHECK THE contents of pubs$alt[1:10] !!!!
  # The combination of enriched part & the unprocessed part
  if (enrich) {
    combined_pubs <- do.call(combining_function, args=list(df.orig, languages, pubs, town, publication_year, cheat_list))
  } else {
    combined_pubs <- clean_publisher(raw_publishers, languages=languages)
    combined_pubs <- harmonize_publisher(combined_pubs, publication_year, languages=languages)
  }
  
  return (combined_pubs)
}
