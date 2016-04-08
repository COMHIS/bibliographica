# TODO sync with update.fields
if (!exists("validate.fields")) {validate.fields <- c("years", "names")}

if ("years" %in% validate.fields) {

  print("Fix publication years")
  # Remove apparent errors: no future publications or publications before historical times
  min.year <- (-2000)
  max.year <- as.numeric(format(Sys.time(), "%Y")) # this.year
  df.preprocessed$publication_year_from[which(df.preprocessed$publication_year_from > max.year)] <- NA
  df.preprocessed$publication_year_from[which(df.preprocessed$publication_year_from < min.year)] <- NA
  df.preprocessed$publication_year_till[which(df.preprocessed$publication_year_till > max.year)] <- NA
  df.preprocessed$publication_year_till[which(df.preprocessed$publication_year_till < min.year)] <- NA

  # Author life years cannot exceed the present year
  # If they do, set to NA
  inds <- which(df.preprocessed$author_birth > max.year)
  if (length(inds) > 0) {
    df.preprocessed[inds, "author_birth"] <- NA
  }
  inds <- which(df.preprocessed$author_death > max.year)
  if (length(inds) > 0) {
     df.preprocessed[inds, "author_death"] <- NA
  }
  # Death must be after birth
  # If this is not the case, set the life years to NA
  inds <- which(df.preprocessed$author_death < df.preprocessed$author_birth)
  if (length(inds) > 0) {
    df.preprocessed[inds, "author_birth"] <- NA
    df.preprocessed[inds, "author_death"] <- NA
  }
}

# Author name validation is rather time-consuming
if ("names" %in% validate.fields) {
  print("Validate author names. Set non-valid names to NA")
  v <- validate_names(df.preprocessed$author_name, "full")
  discard.inds <- !v$valid
  
  # Save discarded names for later analysis
  discarded.author.table <- rev(sort(table(as.character(df.preprocessed$author[discard.inds]))))
  discarded.author.firstnames <- v$invalid.first
  discarded.author.lastnames <- v$invalid.last  

  # Remove discarded names from the list
  df.preprocessed$author_name[discard.inds] <- NA
}

