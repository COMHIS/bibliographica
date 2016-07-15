#' @title Harmonize Publisher Fennica
#' @description Main handler for publisher fields for Fennica.
#' @param df.orig Data frame with raw data, assuming the place, year fields are already polished.
#' @return A vector with polished entries.
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
harmonize_publisher_fennica <- function (df.orig,
			              languages = "english",
				      enrich = FALSE,
			              additional_harmonizing_function = NA,
				      combining_function = NULL,
				      enrichment_function = NULL,
				      cheat_list = NULL) {

  # TODO: consider unique entries only

  # TODO this might be overlapping with polish_publisher
  # the generic function which was called before the present function
  # We may want to skip this with Fennica, not sure
  combined_pubs <- harmonize_publisher(combined_pubs,
				       df.orig$publication_year,
				       languages = languages)


  # Init data.frame
  df <- data.frame(list(row.index = 1:nrow(df.orig)))
  pubs <- data.frame(alt  = character(length = nrow(df.orig)),
                     pref = character(length = nrow(df.orig)),
		     match_method = integer(length=nrow(df.orig)),
		     stringsAsFactors = FALSE)

  enriched_pubs <- data.frame(
    		  alt  = character(length = 0),
    		  pref = character(length = 0),
		  match_methods = character(length = 0),
		  stringsAsFactors = FALSE)

  message("The enrichment part")
  # TODO: enrichments should be in a separate function for clarity,
  # as with the other fields in the pipeline.
  # But this is ok an very useful for now

  # Fennica specific
  # Additional harmonizing: if there's stuff in $corporate -field,
  # which doesn't match with Finto
  # TODO: Separate catalog specific parts outside of bibliographica
  if (!is.na(additional_harmonizing_function) &&
      ("corporate" %in% names(df.orig))) {
      inds <- which(!is.na(df.orig$corporate))
      additionally_harmonized <- do.call(additional_harmonizing_function,
    			         list(df.orig$corporate[inds]))
      pubs$alt[inds]  <- additionally_harmonized$orig
      pubs$pref[inds] <- additionally_harmonized$name
      pubs$match_method[inds] <- 4
  }

  # Fennica specific		  
  if (enrich) {
    enriched_pubs <- do.call(enrichment_function,
    		               args = list(df.orig,
			       	           cheat_list = cheat_list,
		       	 	           languages = languages))
  }

  pubs$alt[which(enriched_pubs$alt != "")] <- enriched_pubs$alt[which(enriched_pubs$alt != "")]

  # Fennica specific
  if (enrich) {
    combined_pubs <- do.call(combining_function,
         args = list(df.orig, languages, pubs,
      		 df.orig$publication_place,
      		 df.orig$publication_year,
		 cheat_list))
  } 

  # Convert S.N. into NA and Author into <Author>
  f <- system.file("extdata/NA_publishers.csv", package="bibliographica")
  synonymes <- read.csv(file = f, sep = "\t", fileEncoding = "UTF-8")
  harmonized_pubs <- map(combined_pubs$mod, synonymes, mode = "recursive")

  # In fact only necessary to return mod
  return(harmonized_pubs)

}
