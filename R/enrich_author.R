#' @title Enrich Author Field
#' @description Enrich author field.
#' @param df Preprocessed data.frame
#' @return Augmented data.frame
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df2 <- enrich_authoragecount(df)}
#' @keywords utilities
enrich_author <- function(df) {

  message("Enriching author fields..")

    # Custom table toaAugmen missing life years
    life.info <- read.csv(system.file("extdata/author_info.csv", package = "bibliographica"), stringsAsFactors = FALSE, sep = "\t")

    # Custom table to harmonize multiple author name variants
    f <- system.file("extdata/ambiguous-authors.csv", package = "bibliographica")
    ambiguous.authors <- read_mapping(f, mode = "list", sep = ";", self.match = FALSE, include.lowercase = FALSE, fast = TRUE)
    
    # Combine synonymous authors; augment author life years where missing etc.
    aa <- augment_author(df, life.info, ambiguous.authors)

    df <- aa
    rm(aa)

  # -------------------------------------------------------------------

  # TODO improve: many names are missing gender;
  # and time variation in name-gender mappings not counted
  message("Add estimated author genders")
  # Filter out names that are not in our input data
  # (this may speed up a bit)
  first.names <- pick_firstname(df$author_name, format = "last, first", keep.single = TRUE)

  # First use gender mappings from the ready-made table
  if (!exists("gendermap.file") || is.null(gendermap.file)) {
    gendermap.file = system.file("extdata/gendermap.csv", package = "bibliographica")
  }
  gendermap <- read_mapping(gendermap.file, sep = "\t", from = "name", to = "gender")
  df$author_gender <- get_gender(first.names, gendermap)

  message("Custom name-gender mappings to resolve ambiguous cases")
  # Consider the custom table as primary  
  # ie override other matchings with it
  custom <- gender_custom()
  g <- get_gender(first.names, custom)
  inds <- which(!is.na(g))
  df$author_gender[inds] <- g[inds]

  message("Add genders from the generic author info custom table")
  tab <- read.csv(system.file("extdata/author_info.csv", package = "bibliographica"), sep = "\t")
  g <- map(df$author_name, tab, from = "author_name", to = "author_gender", remove.unknown = TRUE)
  inds <- which(!is.na(g))
  df$author_gender[inds] <- g[inds]

  # Author life - make sure this is in numeric format
  df$author_birth <- as.numeric(as.character(df$author_birth))
  df$author_death <- as.numeric(as.character(df$author_death))  

  message("Author age on the publication year, when both info are available")
  inds <- which(df$publication_year_from < df$author_death & !is.na(df$author_birth))
  df$author_age <- rep(NA, nrow(df))
  df$author_age[inds] <- df$publication_year_from[inds] - df$author_birth[inds]
  # Remove negative author ages
  # FIXME: taking these to the final summaries could help to spot errors
  # df$author_age[df$author_age < 0] <- NA

  return (df)

}

