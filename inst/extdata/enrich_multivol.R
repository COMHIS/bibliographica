print("Multi-vol docs that have < 10 pages or no pages")
# those with <10 pages are typically ones with
# only plate page information and missing real page information
# as an approximation we can omit the plate information 
# as it is much smaller than the real page count
# Exclude docs with >10 volumes since these are likely not
# following average volume-wise page counts
inds <- which(((df.preprocessed$volcount > 1 & df.preprocessed$volcount <= 10) | 
     				     !is.na(df.preprocessed$volnumber)) & 
	    (df.preprocessed$pagecount <= 10 | is.na(df.preprocessed$pagecount)) & 
               !is.na(df.preprocessed$gatherings) & 
	       !df.preprocessed$gatherings %in% c("1to", "2small", "2to", "2long", "4small", "4long", "4to"))

g <- df.preprocessed$gatherings[inds]
v <- df.preprocessed$volcount[inds] # number of vols
p <- df.preprocessed$pagecount[inds]

# Those with volume number constitute one item from multi-volume series
# so use 1 as volume count for these
inds2 <- which(is.na(v) & !is.na(df.preprocessed$volnumber[inds])) 
v[inds2] <- 1

print("Pick the estimated page counts per vol separately for each doc size")
pages.per.vol <- mean.pagecounts.multivol[match(g, mean.pagecounts.multivol$doc.dimension), ]$median.pages.multivol

print("Add estimated total page counts for all docs")
indsa <- inds
df.preprocessed[inds, "pagecount"] <- v * pages.per.vol
# Assuming that page counts <10 are in fact cover pages, add these to the estimated page count
inds2 <- which(p <= 10 & !is.na(p))
df.preprocessed[inds2, "pagecount"] <- df.preprocessed[inds2, "pagecount.orig"] + v[inds2] * p[inds2]
