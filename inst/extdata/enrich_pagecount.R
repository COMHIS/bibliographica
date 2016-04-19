
print("Add volume info where missing")
if (!"volcount" %in% names(df.preprocessed))  { df.preprocessed$volcount <- rep(1, nrow(df.preprocessed))   }
if (!"volnumber" %in% names(df.preprocessed)) { df.preprocessed$volnumber <- rep(NA, nrow(df.preprocessed)) }

# Always assume single volume if no volume info is given
df.preprocessed$volcount[is.na(df.preprocessed$volcount) & is.na(df.preprocessed$volnumber)] <- 1

# --------------------------------------------------------------------------

print("Estimate total pages for the docs where it is missing")
df.preprocessed$pagecount.orig <- df.preprocessed$pagecount

# --------------------------------------------------------------------------

print("Calculate average page counts based on available data")
# TODO make a function that quickly returns this. No need to precalculate it.
source(system.file("extdata/mean_pagecounts.R", package = "bibliographica"))

# --------------------------------------------------------------------------

# Identify issues that are missing pagecount
# and add page count estimates
issue <- is.issue(df.preprocessed)
inds <- which(issue & is.na(df.preprocessed$pagecount))
df.preprocessed[inds, "pagecount"] <- estimate_pages_issue(df.preprocessed[inds,])

# Identify multi-vol docs
multivol <- is.multivol(df.preprocessed)
# .. and then take only those without page count
# ... also consider docs with <10 pages having missing page info as
# these are typically ones with only some plate page information and
# missing real page information
inds <- multivol & (is.na(df$pagecount) | df$pagecount <= 10)
df.preprocessed[inds, "pagecount"] <- estimate_pages_multivol(df.preprocessed[inds,])

# Single-vol docs missing pagecount
singlevol <- is.singlevol(df.preprocessed)
inds <- singlevol & is.na(df$pagecount)

TODO
g <- as.character(df.preprocessed$gatherings[inds])
print("Pick the estimated page counts per vol separately for each doc size")
inds2 = match(g, as.character(mean.pagecounts.univol$doc.dimension))
pages.per.vol <- mean.pagecounts.univol[inds2, ]$median.pages.singlevol
print("Add estimated total page counts for all docs")
df.preprocessed[inds, "pagecount"] <- 1 * pages.per.vol


# -----------------------------------------------

# 1to pitäisi aina olla tasan 2 sivua.
# Eli yksi sheet, broardside tai 1to (kutsutaan millä tahansa nimellä),
# mutta siinä on aina yksi lehti (ja siten kaksi sivua).
# Näin ollen kaikki merkinnät joissa >2 sivua voisi siirtää 2fo kategoriaan.
df.preprocessed[which(df.preprocessed$gatherings == "1to" & df.preprocessed$pagecount > 2), "gatherings"] <- "2fo"



