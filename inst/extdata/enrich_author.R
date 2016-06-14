message("Enriching author fields..")

  # Custom table toaAugmen missing life years
  life.info <- read.csv(system.file("extdata/author_info.csv", package = "bibliographica"), stringsAsFactors = FALSE, sep = "\t")

  # Custom table to harmonize multiple author name variants
  f <- system.file("extdata/ambiguous-authors.csv", package = "bibliographica")
  ambiguous.authors <- read_mapping(f, mode = "list", sep = ";", self.match = FALSE, include.lowercase = FALSE, fast = TRUE)
  
  # Combine synonymous authors; augment author life years where missing etc.
  aa <- augment_author(df.preprocessed, life.info, ambiguous.authors)

  df.preprocessed <- aa
  rm(aa)


# -------------------------------------------------------------------

# TODO improve: many names are missing gender;
# and time variation in name-gender mappings not counted
message("Add estimated author genders")
# Filter out names that are not in our input data
# (this may speed up a bit)
first.names <- pick_firstname(df.preprocessed$author_name, format = "last, first", keep.single = TRUE)

# First use gender mappings from the ready-made table
if (!exists("gendermap.file") || is.null(gendermap.file)) {
  gendermap.file = system.file("extdata/gendermap.csv", package = "bibliographica")
}
gendermap <- read_mapping(gendermap.file, sep = "\t", from = "name", to = "gender")
df.preprocessed$author_gender <- get_gender(first.names, gendermap)

print("Custom name-gender mappings to resolve ambiguous cases")
# Consider the custom table as primary  
# ie override other matchings with it
custom <- gender_custom()
g <- get_gender(first.names, custom)
inds <- which(!is.na(g))
df.preprocessed$author_gender[inds] <- g[inds]

print("Add genders from the generic author info custom table")
tab <- read.csv(system.file("extdata/author_info.csv", package = "bibliographica"), sep = "\t")
g <- map(df.preprocessed$author_name, tab, from = "author_name", to = "author_gender", remove.unknown = TRUE)
inds <- which(!is.na(g))
df.preprocessed$author_gender[inds] <- g[inds]


# -------------------------------------------------------------------

