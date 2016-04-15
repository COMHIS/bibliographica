print("Single-vol docs")
inds <- which(df.preprocessed$volcount == 1 &
              is.na(df.preprocessed$volnumber) & 
	      is.na(df.preprocessed$pagecount) & 
              !is.na(df.preprocessed$gatherings) & 
	      !df.preprocessed$gatherings %in% c("1to", "2small", "2to", "2long"))
g <- as.character(df.preprocessed$gatherings[inds])

print("Pick the estimated page counts per vol separately for each doc size")
inds2 = match(g, as.character(mean.pagecounts.univol$doc.dimension))

pages.per.vol <- mean.pagecounts.univol[inds2, ]$median.pages.singlevol

print("Add estimated total page counts for all docs")
indsb <- inds
df.preprocessed[inds, "pagecount"] <- 1 * pages.per.vol
