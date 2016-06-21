harmonize_publisher_main <- function (datasource, df.orig=NULL, testing_max=NULL, output.file=NULL) {
  
  enrich <- FALSE

  if (is.null(df.orig)) {
    df.orig <- read_bibliographic_metadata(source.data.file)
  }
  
  # TODO: Get necessary function names, tables etc. from a single csv-file!
  additional_harmonizing_function <- NA
  
  if (datasource == "fennica") {
    source.data.file <- "data/fennica.csv"
    languages <- c("finnish", "latin", "swedish")
    enrich <- TRUE
    enrichment_function <- "harmonize_publisher_fennica"
    additional_harmonizing_function <- "harmonize_corporate_Finto"
    combining_function <- "combine_publisher_fennica"
    
    publication_year <- polish_year_of_publication(df.orig$publication_time)
    town <- polish_place(df.orig$publication_place)
    cheat_list <- cheat_publishers()
    inds <- which(!is.na(df.orig$corporate))
    
    
  } else if (datasource == "kungliga") {
    languages <- c("swedish")
    source.data.file <- "data/kungliga.csv"
    enrich <- FALSE
    inds <- integer(length(0))
    publication_year <- polish_year_of_publication(df.orig$publication_time)
    raw_publishers <- df.orig$publisher
    raw_publishers[which(is.na(raw_publishers))] <- df.orig$corporate[which(is.na(raw_publishers))]
  }
  
  output.folder <- "Output/"
  
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
  if (!is.na(additional_harmonizing_function)) {
    additionally_harmonized <- do.call(additional_harmonizing_function, list(df.orig$corporate[inds]))
    pubs$alt[inds] <- additionally_harmonized$orig
    pubs$pref[inds] <- additionally_harmonized$name
    pubs$match_method[inds] <- 4
  }
  
  # The enrichment part
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
  
  
  if (!is.null(output.file)) {
    write_xtable(combined_pubs, paste0(output.folder, output.file), count = FALSE)
  }
  return (combined_pubs)
}
