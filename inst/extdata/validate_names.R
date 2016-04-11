print("Validate author names. Set non-valid names to NA")
v <- validate_names(df.preprocessed$author_name, "full")
discard.inds <- !v$valid
  
# Save discarded names for later analysis
discarded.author.table <- rev(sort(table(as.character(df.preprocessed$author[discard.inds]))))
discarded.author.firstnames <- v$invalid.first
discarded.author.lastnames <- v$invalid.last  

# Remove discarded names from the list
df.preprocessed$author_name[discard.inds] <- NA