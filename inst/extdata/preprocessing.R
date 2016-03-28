# Preprocess all fields at once
res <- polish_all(df.orig, file = "df.preprocessed.RData")
df.preprocessed <- df.preprocessed0 <- res$df.preprocessed
conversions <- res$conversions
saveRDS(df.preprocessed0, "df0.Rds")
saveRDS(conversions, "conversions.Rds")
#save(df.preprocessed, file = "df.preprocessed.RData", compress = "xz")
