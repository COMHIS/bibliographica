
preprocess_data <- function(preprocessing.data, df.orig, languages, mc.cores = 1) {

  # mc.cores required by polish_all()

  df.preprocessed <- preprocessing.data$df.preprocessed
  update.fields <-   preprocessing.data$update.fields
  conversions <-     preprocessing.data$conversions

  check <- "preprocess"
  # debug variable? -vv

  message("Preprocess selected original fields")
  
  res <- polish_all(df.orig, fields = update.fields, 
      mc.cores = mc.cores,
      conversions = conversions, languages = languages)

  if (is.null("df.preprocessed") || !exists("df.preprocessed")) {
    # !exists carryover from previous version. Can be removed in future? -vv

    conversions <- res$conversions
    preprocessing.times <- res$preprocessing.times
    df.preprocessed <- res$df.preprocessed  

  } else {

    # Replace old versions with the updated ones (if any)
    conversions[update.fields] <- res$conversions[update.fields]

    if (!exists("preprocessing.times")) {
      preprocessing.times <- res$preprocessing.times
    }
    preprocessing.times[update.fields] <- res$preprocessing.times[update.fields]

    upf <- unlist(conversions[update.fields])
    df.preprocessed[, upf] <- res$df.preprocessed[, upf]

  }

  saveRDS(df.preprocessed, "df0.Rds", compress = TRUE)
  saveRDS(conversions, "conversions.Rds", compress = TRUE)

  output_preprocessing_times_pdf(preprocessing.times)

  preprocessed.data <- list(df.preprocessed = df.preprocessed,
                            update.fields = update.fields,
                            conversions = conversions) 

  return (preprocessed.data)
}


output_preprocessing_times_pdf <- function(preprocessing.times) {
  # Processing times
  library(magrittr)
  library(ggplot2)
  theme_set(theme_bw(20))
  dft <- data.frame(Field = factor(names(preprocessing.times)),
              Time = preprocessing.times)
  dft %<>% arrange(Time)
  dft$Field <- factor(dft$Field, levels = as.character(dft$Field))
  p <- ggplot(dft, aes(x = Field, y = Time)) +
       geom_bar(stat = "identity") +
       ylab("Time (Minutes)") +
       coord_flip() +
       ggtitle("Processing times")
  # print(p)
  pdf("processingtimes.png")
  print(p)
  dev.off()
}
