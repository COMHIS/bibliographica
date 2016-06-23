#' @title Harmonize publisher Fennica
#' @description Harmonizes publisher names and enriches them with Finto information
#' @param df.orig Raw data from csv
#' @param cheat_list Data frame of alt, pref: alternative spellings and their counterpart the preferred spelling
#' @param languages A vector of languages which are used in detecting relation keywords
#' @return Data frame with alt, pref, match_methods
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @examples # harmonize_publisher_fennica(df.orig, cheat_list, languages=c("finnish", "swedish", "latin"))
#' @keywords utilities
harmonize_publisher_fennica <- function(df.orig, cheat_list, languages=c("english")) {

  # First: Take the publication year  
  publication_year <- polish_year_of_publication(df.orig$publication_time)
  
  # Get Finto data from field 710a ($corporate)
  publisher <- harmonize_corporate_Finto(df.orig$corporate)
  town <- polish_place(df.orig$publication_place)
  
  # Get remaining values from other fields
  inds <- which(!is.na(publisher$name))
  publisher$name[-inds] <- clean_publisher(df.orig$publisher[-inds], languages=languages)
  publisher$orig[-inds] <- as.character(df.orig$publisher[-inds])
  publisher$town[-inds] <- df.orig$publication_place[-inds]

  # Test if misspelling can be corrected using corporate field values for all the corresponding publisher values
  known_indices <- which(!is.na(publisher$name))
  unknown_indices <- which(is.na(publisher$name))
  known_names <- clean_publisher(df.orig$publisher[known_indices], languages=languages)
  unknown_names <- unique(clean_publisher(df.orig$publisher[unknown_indices], languages=languages))
  corrected_names <- publisher$name[known_indices]

  # Cheat list contains every bit of info from Finto XML
  #cheat_list <- cheat_publishers()
  Finto_years <- data.frame(year_from=cheat_list$year_from, year_till=cheat_list$year_till, stringsAsFactors = FALSE)
  # NB! Add town synonyms!
  Finto_town <- polish_place(publisher$town)
  all_names <- clean_publisher(publisher$name, languages=c("finnish"))

  # TODO: this is slow - to optimize
  Finto_comp <- extract_personal_names(cheat_list$alt, languages=c("finnish", "latin", "swedish"))
  all_names <- extract_personal_names(all_names, c("finnish", "latin", "swedish"))
  
  # Check against Finto, if there's a preferred way in outputting the publisher name
  # Included are also publication place & year
  # Typos are allowed to a small extent
  Finto_pubs <- get_publishers_Finto(Finto_corrected = cheat_list$pref, 
                            Finto_comp = Finto_comp,
                            all_names = all_names, 
                            known_inds = inds,
                            Finto_town = cheat_list$town,
                            unknown_town = town,
                            publication_year = publication_year,
                            Finto_years = Finto_years)

  return (Finto_pubs)
}
  