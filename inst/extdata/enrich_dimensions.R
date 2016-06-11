message("Enriching dimensions..")

print("Estimate missing dimension info")
# Estimate missing dimension info
dim.orig <- df.preprocessed[, c("gatherings.original", "width.original", "height.original", "obl.original")]
names(dim.orig) <- gsub("\\.original$", "", names(dim.orig))

# Estimating missing dimension info straight from data.
# TODO: compare to our ready made sheets -> there are differences, why ???
# TODO: store the estimates as table
# TODO: augment values that still remain missing, using our ready made sheet
# TODO: Later, also account for year and publication place if feasible
#       as the sizes may vary
# Averages from original data

library(dplyr)
# Mean dimensions for each gatherings
dim.estimates <- dim.orig %>%
   group_by(gatherings, obl) %>%
   summarize(
     mean.width = mean(width, na.rm = TRUE),
     mean.height = mean(height, na.rm = TRUE),
     n = n()
   )
dim.estimates.orig <- dim.estimates


# For long with NA, use the standard to (12long -> 12to)
long <- unique(dim.estimates$gatherings[grep("long", dim.estimates$gatherings)])
for (g in long) {

  wlong <- filter(dim.estimates, gatherings == g)$mean.width
  hlong <- filter(dim.estimates, gatherings == g)$mean.height 
  if (length(wlong) == 0) {wlong <- NA}
  if (length(hlong) == 0) {hlong <- NA}  

  gnum <- gsub("long", "", g)
  ind <- grep(paste("^", gnum, ".o$", sep = ""), unique(dim.estimates$gatherings))
  gstandard <- as.character(unique(dim.estimates$gatherings)[[ind]])
  ind2 <- which(dim.estimates$gatherings == g & dim.estimates$obl == FALSE)

  if (is.na(wlong)) {
    wlong <- filter(dim.estimates, gatherings == gstandard & obl == FALSE)$mean.width  
    dim.estimates[ind2, "mean.width"] <- wlong    
  }

  if (is.na(hlong)) {
    hlong <- filter(dim.estimates, gatherings == gstandard & obl == FALSE)$mean.height
    dim.estimates[ind2, "mean.height"] <- hlong    
  }

}

# For obl with NA, use the reverse of non-obl
for (g in unique(dim.estimates$gatherings)) {

  wobl <- filter(dim.estimates, gatherings == g & obl == TRUE)$mean.width
  hobl <- filter(dim.estimates, gatherings == g & obl == TRUE)$mean.height 

  if (length(wobl) == 0) {wobl <- NA}
  if (length(hobl) == 0) {hobl <- NA}  

  h <- filter(dim.estimates, gatherings == g & obl == FALSE)$mean.height
  if (is.na(wobl) && length(h) > 0) {
    wobl <- h
    inds <- which(dim.estimates$gatherings == g & dim.estimates$obl == TRUE)
    dim.estimates[inds, "mean.width"] <- wobl    
  }

  w <- filter(dim.estimates, gatherings == g & obl == FALSE)$mean.width
  if (is.na(hobl) && length(w) > 0) {  
    hobl <- w
    inds <- which(dim.estimates$gatherings == g & dim.estimates$obl == TRUE)
    dim.estimates[inds, "mean.height"] <- hobl    
  }

}

# Ready-made custom sheets
#dim.info <- dimension_table()
#sheet.dim.tab <- sheet_area()
dim.estimated <- augment_dimension_table(dim.orig, verbose = FALSE)
# Remove earlier versions of these fields
if (any(names(dim.estimated) %in% names(df.preprocessed))) {
  df.preprocessed <- df.preprocessed[, -which(names(df.preprocessed) %in% names(dim.estimated))]
}

# Merge
df.preprocessed <- cbind(df.preprocessed, dim.estimated)  

