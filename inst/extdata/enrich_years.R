message("Enriching publication years..")

  print("Add publication year")
  # Use from field; if from year not available, then use till year
  df.preprocessed$publication_year <- df.preprocessed$publication_year_from
  inds <- which(is.na(df.preprocessed$publication_year))
  df.preprocessed$publication_year[inds] <- df.preprocessed$publication_year_till[inds]

  print("Add publication decade")
  df.preprocessed$publication_decade <- floor(df.preprocessed$publication_year/10) * 10 # 1790: 1790-1799
