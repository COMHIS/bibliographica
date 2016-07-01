#' @title Prepare combined catalogs
#' @description Preprocesses and combines two catalogs (not every field)
#' @param df.fennica Raw Fennica data
#' @param df.kungliga Raw Kungliga data
#' @param first_word_count Number of words in the beginning of a title needed to make a match
#' @return Data frame of core fields
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
prepare_combined_catalogs <- function (df.fennica, df.kungliga, first_word_count=5) {
  
  fennica_authors <- polish_author(df.fennica$author_name)
  fennica_years <- polish_years(df.fennica$publication_time)
  fennica_titles <- polish_title(df.fennica$title)
  fennica_town <- polish_place(df.fennica$publication_place)
  fennica_dissertations <- df.fennica$note_dissertation
  
  kungliga_authors <- polish_author(df.kungliga$author_name)
  kungliga_years <- polish_years(df.kungliga$publication_time)
  kungliga_titles <- polish_title(df.kungliga$title)
  kungliga_town <- polish_place(df.kungliga$publication_place)
  kungliga_dissertations <- df.kungliga$note_dissertation
  
  pattern = paste0("^([^ ]* ){", first_word_count, "}")
  f_short_titles <- str_extract(fennica_titles, pattern=pattern)
  inds <- which(is.na(f_short_titles))
  f_short_titles[inds] <- fennica_titles[inds]
  k_short_titles <- str_extract(kungliga_titles, pattern=pattern)
  inds <- which(is.na(k_short_titles))
  k_short_titles[inds] <- kungliga_titles[inds]
  
  processed_fennica <- data.frame(catalog="fennica", 
                                  author=fennica_authors, 
                                  short_title=f_short_titles, 
                                  full_title=fennica_titles, 
                                  town=fennica_town, 
                                  from=fennica_years$from, 
                                  till=fennica_years$till, 
                                  diss=fennica_dissertations, 
                                  stringsAsFactors = FALSE)
  
  processed_kungliga <- data.frame(catalog="kungliga",
                                   author=kungliga_authors, 
                                   short_title=k_short_titles, 
                                   full_title=kungliga_titles, 
                                   town=kungliga_town, 
                                   from=kungliga_years$from, 
                                   till=kungliga_years$till, 
                                   diss=kungliga_dissertations,  
                                   stringsAsFactors = FALSE)
  
  l <- (nrow(processed_fennica) + nrow(processed_kungliga))
  
  # Combine the two catalogs
  combined <- data.frame(catalog_index=integer(l),
                         catalog=character(l), 
                         author=character(l), 
                         short=character(l), 
                         title=character(l), 
                         town=character(l), 
                         from=integer(l), 
                         till=integer(l), 
                         dissertation=character(l),
                         dup_cluster=integer(l), 
                         dup_index=integer(l), 
                         remove=character(l), 
                         stringsAsFactors = FALSE)
  
  # Fill the combined list with values
  inds <- 1:nrow(processed_fennica)
  combined[inds,1] <- inds
  combined[inds,2:9] <- processed_fennica[1:8]
  inds <- (nrow(processed_fennica)+1):nrow(combined)
  combined[inds,1] <- 1:nrow(processed_kungliga)
  combined[inds,2:9] <- processed_kungliga[,1:8]
  
  

  return (combined)
}
