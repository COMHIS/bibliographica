message("Enriching author fields..")

  # Custom table toaAugmen missing life years
  life.info <- read.csv(system.file("extdata/author_info.csv", package = "bibliographica"), stringsAsFactors = FALSE, sep = "\t")

  # Custom table to harmonize multiple author name variants
  f <- system.file("extdata/ambiguous-authors.csv", package = "bibliographica")
  ambiguous.authors <- read_synonymes(f, mode = "list", sep = ";", self.match = FALSE, include.lowercase = FALSE)
  
  # Combine synonymous authors; augment author life years where missing etc.
  aa <- augment_author(df.preprocessed, life.info, ambiguous.authors)

  df.preprocessed <- aa
  rm(aa)


# -------------------------------------------------------------------

# TODO improve: many names are missing gender;
# and time variation in name-gender mappings not counted
message("Add estimated author genders")
# Assumes that the author name is in the form "Last, First".

# Filter out names that are not in our input data
# (this may speed up a bit)
first.names <- pick_firstname(df.preprocessed$author_name, format = "last, first")
gendermap <- gender_map() 
gendermap <- gendermap %>% filter(name %in% unique(unlist(strsplit(first.names, " "))))
df.preprocessed$author_gender <- get_gender(first.names, gendermap)

# -------------------------------------------------------------------

