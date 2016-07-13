check <- "preprocess"

# Preprocess selected original fields
res <- polish_all(df.orig, fields = update.fields, 
	  mc.cores = mc.cores,
	  conversions = conversions, catalog = catalog)

# -------------------------------------------------------

if (!exists("df.preprocessed")) {

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


