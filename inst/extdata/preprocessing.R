# If update fields is provided, then look for preprocessed file
if (exists("update.fields") && ("df0.Rds" %in% dir())) {
  df.orig <- df.orig # readRDS("df.raw.Rds")
  df.preprocessed <- readRDS("df0.Rds")  
  conversions <- readRDS("conversions.Rds")
} else {
  df.orig <- df.orig
  conversions <- list()
  update.fields <- NULL
}

# Preprocess selected original fields 
res <- polish_all(df.orig, fields = update.fields,
          file = "df.preprocessed.RData", mc.cores = mc.cores,
	  conversions = conversions)

# Apply updates
conversions <- res$conversions
preprocessing.times <- res$preprocessing.times
upf <- unlist(conversions[[update.fields]])
if (length(upf) > 0) {
  df.preprocessed[, upf] <- res$df.preprocessed[, upf]
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


