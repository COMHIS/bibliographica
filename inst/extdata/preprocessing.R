preprocess_data <- function(preprocessing.data, df.orig, languages) {

  df.preprocessed <- preprocessing.data$df.preprocessed
  update.fields   <- preprocessing.data$update.fields
  conversions     <- preprocessing.data$conversions

  message("Preprocess selected original fields")
  
  res <- polish_all(df.orig,
      fields = update.fields, 
      conversions = conversions,
      languages = languages)

    conversions <- res$conversions
    preprocessing.times <- res$preprocessing.times
    df.preprocessed <- res$df.preprocessed  

  saveRDS(df.preprocessed, "data/unified/polished/df0.Rds", compress = TRUE)
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
