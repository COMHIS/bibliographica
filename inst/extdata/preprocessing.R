# Preprocess all fields at once
res <- polish_all(df.orig, file = "df.preprocessed.RData")
df.preprocessed <- df.preprocessed0 <- res$df.preprocessed
conversions <- res$conversions
saveRDS(df.preprocessed0, "df0.Rds", compress = TRUE)
saveRDS(conversions, "conversions.Rds", compress = TRUE)

# Visualize processing times for the different fields
source("processingtimes.R")

