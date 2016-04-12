message("Enriching author fields..")

print("Enrich author info")
  # Life years + author_unique field

  life.info <- read.csv(system.file("extdata/author_info.csv", package = "bibliographica"), stringsAsFactors = FALSE, sep = "\t")

  f <- system.file("extdata/ambiguous-authors.csv", package = "bibliographica")
  ambiguous.authors <- read_synonymes(f, mode = "list", sep = ";", include.original = FALSE, include.lowercase = FALSE)
  
  # Combine synonymous authors; augment author life years where missing etc.
  aa <- augment_author(df.preprocessed, life.info, ambiguous.authors)
  df.preprocessed <- aa
  rm(aa)


# -------------------------------------------------------------------

# TODO improve: many names are missing gender;
# and time variation in name-gender mappings not counted
print("Estimate author genders")
# Assumes that the author name is in the form "Last, First".
df.preprocessed$author_gender <- get_gender(pick_firstname(df.preprocessed$author_name, format = "last, first"))

# -------------------------------------------------------------------

