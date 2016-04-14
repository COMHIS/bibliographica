print("Calculate average page counts based on available data")
# TODO make a function that quickly returns this. No need to precalculate it.
print("Average pagecounts")
dftmp <- df.preprocessed
if (!"volcount" %in% names(dftmp)) {dftmp$volcount <- 1}
if (!"volnumber" %in% names(dftmp)) {dftmp$volnumber <- 1}
mean.pagecounts.multivol <- mean_pagecounts_multivol(dftmp) 
mean.pagecounts.univol <- mean_pagecounts_univol(dftmp) 
mean.pagecounts.issue <- mean_pagecounts_issue(dftmp) 

print("..join..")
mean.pagecounts <- full_join(mean.pagecounts.univol, mean.pagecounts.multivol, by = "doc.dimension")
mean.pagecounts <- full_join(mean.pagecounts, mean.pagecounts.issue, by = "doc.dimension")
mean.pagecounts$doc.dimension <- factor(mean.pagecounts$doc.dimension,
			      levels = levels(mean.pagecounts.univol$doc.dimension))
# write.table(mean.pagecounts, file = paste(output.folder, "estimated_page_counts.csv", sep = ""),
#  quote = F, row.names = F, sep = ",")

# -----------------------------------------------------------------------

print("Estimate total pages for the docs where it is missing")
df.preprocessed$pagecount.orig <- df.preprocessed$pagecount

# Gatherings 1to-4to (any number of vols) or >8to (with >10 vols)
# with missing page information are assumed to be 'issues'
# and hence different estimated page counts are applied 
inds <- which(is.na(df.preprocessed$pagecount) & ((df.preprocessed$gatherings %in% c("1to", "2small", "2to", "2long", "4small", "4to", "4long")) | (!df.preprocessed$gatherings %in% c("1to", "2small", "2to", "2long", "4small", "4to", "4long") & df.preprocessed$volcount > 10)))
g <- df.preprocessed$gatherings[inds]
v <- df.preprocessed$volcount[inds] # number of vols
# Those with volume number constitute one item from multi-volume series
# so use 1 as volume count for these
inds2 <- which(is.na(v) & !is.na(df.preprocessed$volnumber[inds])) 
v[inds2] <- 1

print("Pick the estimated page counts per vol separately for each doc size")
pages.per.vol <- mean.pagecounts.issue[match(g, mean.pagecounts.issue$doc.dimension), ]$median.pages.issue

print("Add estimated total page counts for issues")
indsc <- inds
df.preprocessed[inds, "pagecount"] <- v * pages.per.vol

# -----------------------------------------------------------

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

# ----------------------------------------------

print("Single-vol docs")
inds <- which(df.preprocessed$volcount == 1 &
              is.na(df.preprocessed$volnumber) & 
	      is.na(df.preprocessed$pagecount) & 
              !is.na(df.preprocessed$gatherings) & 
	      !df.preprocessed$gatherings %in% c("1to", "2small", "2to", "2long"))
g <- as.character(df.preprocessed$gatherings[inds])

print("Pick the estimated page counts per vol separately for each doc size")
pages.per.vol <- mean.pagecounts.univol[match(g, as.character(mean.pagecounts.univol$doc.dimension)), ]$median

print("Add estimated total page counts for all docs")
indsb <- inds
df.preprocessed[inds, "pagecount"] <- 1 * pages.per.vol

# 1to pitäisi aina olla tasan 2 sivua.
# Eli yksi sheet, broardside tai 1to (kutsutaan millä tahansa nimellä),
# mutta siinä on aina yksi lehti (ja siten kaksi sivua).
# Näin ollen kaikki merkinnät joissa >2 sivua voisi siirtää 2fo kategoriaan.
df.preprocessed[which(df.preprocessed$gatherings == "1to" & df.preprocessed$pagecount > 2), "gatherings"] <- "2fo"



