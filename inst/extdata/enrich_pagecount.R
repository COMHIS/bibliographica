print("Calculate average page counts based on available data")
# TODO make a function that quickly returns this. No need to precalculate it.
print("Average pagecounts")
if (!"volcount" %in% names(df.preprocessed))  {df.preprocessed$volcount <- rep(1, nrow(df.preprocessed))}
if (!"volnumber" %in% names(df.preprocessed)) {df.preprocessed$volnumber <- rep(NA, nrow(df.preprocessed))}
source(system.file("extdata/mean_pagecounts.R", package = "bibliographica"))

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

source(system.file("extdata/enrich_multivol.R", package = "bibliographica"))
source(system.file("extdata/enrich_singlevol.R", package = "bibliographica"))

# -----------------------------------------------

# 1to pitäisi aina olla tasan 2 sivua.
# Eli yksi sheet, broardside tai 1to (kutsutaan millä tahansa nimellä),
# mutta siinä on aina yksi lehti (ja siten kaksi sivua).
# Näin ollen kaikki merkinnät joissa >2 sivua voisi siirtää 2fo kategoriaan.
df.preprocessed[which(df.preprocessed$gatherings == "1to" & df.preprocessed$pagecount > 2), "gatherings"] <- "2fo"



