
# Preprocess selected original fields 
res <- polish_all(df.orig, fields = update.fields,
          file = "df.preprocessed.RData", mc.cores = mc.cores,
	  conversions = conversions)

# Replace old versions with the updated ones (if any)
conversions <- res$conversions
preprocessing.times <- res$preprocessing.times
if (!is.null(update.fields)) {
  upf <- unlist(conversions[update.fields])
  df.preprocessed[, upf] <- res$df.preprocessed[, upf]
} else {
  # All fields updated by default
  df.preprocessed <- res$df.preprocessed
}

# -----------------------------------------------

saveRDS(df.preprocessed, "df0.Rds", compress = TRUE)
saveRDS(conversions, "conversions.Rds", compress = TRUE)

sessioninfo <- sessionInfo()
saveRDS(sessioninfo, "sessioninfo.Rds", compress = TRUE)

# -----------------------------------------------

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
     #scale_y_log10() +
     ylab("Time (Minutes)") +
     coord_flip() +
     ggtitle("Processing times")
print(p)
pdf("processingtimes.png")
print(p)
dev.off()


