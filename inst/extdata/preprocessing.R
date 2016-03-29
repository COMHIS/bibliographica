# Preprocess all fields at once
res <- polish_all(df.orig, file = "df.preprocessed.RData")
df.preprocessed <- df.preprocessed0 <- res$df.preprocessed
conversions <- res$conversions
preprocessing.times <- res$preprocessing.times

# -----------------------------------------------

saveRDS(df.preprocessed0, "df0.Rds", compress = TRUE)
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


