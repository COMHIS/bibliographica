##
## CALCULATE AVERAGE DOC SIZES FROM THE ORIGINAL ENTRIES
##

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

  if (is.na(wobl)) {
    h <- filter(dim.estimates, gatherings == g & obl == FALSE)$mean.height
    wobl <- h
    inds <- which(dim.estimates$gatherings == g & dim.estimates$obl == TRUE)
    dim.estimates[inds, "mean.width"] <- wobl    
  }

  if (is.na(hobl)) {
    w <- filter(dim.estimates, gatherings == g & obl == FALSE)$mean.width
    hobl <- w
    inds <- which(dim.estimates$gatherings == g & dim.estimates$obl == TRUE)
    dim.estimates[inds, "mean.height"] <- hobl    
  }

}

# --------------------------------------------

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

# -----------------------------------------------------------

print("Publication times")
# Use from field; if from year not available, then use till year
df.preprocessed$publication_year <- df.preprocessed$publication_year_from
inds <- which(is.na(df.preprocessed$publication_year))
df.preprocessed$publication_year[inds] <- df.preprocessed$publication_year_till[inds]

# publication_decade
df.preprocessed$publication_decade <- floor(df.preprocessed$publication_year/10) * 10 # 1790: 1790-1799

# -----------------------------------------------------------

print("Geocoordinates")
#source("geocoordinates.R")
load(system.file("extdata/geonames.RData", package = "bibliographica"))
load(system.file("extdata/places.geonames.RData", package = "bibliographica"))

geoc <- bibliographica::get_geocoordinates(df.preprocessed$publication_place, geonames, places.geonames)
geoc$publication_place <- df.preprocessed$publication_place

# Remove earlier versions of these fields
if (any(names(geoc) %in% names(df.preprocessed))) {
  df.preprocessed <- df.preprocessed[, -which(names(df.preprocessed) %in% names(geoc))]
}
# Merge
df.preprocessed <- cbind(df.preprocessed, geoc)  

# -----------------------------------------------------------------

print("Add publication country")
df.preprocessed$country <- get_country(df.preprocessed$publication_place)$country
# We could standardize country names but problem is that e.g. England, Scotland
# etc are not mapped (as UK). But is potentially useful later.
#devtools::install_github("dsself/standardizecountries")
# library(standard)
# df$publication_country2 <- country_name(df$publication_country)
# df$publication_country.code <- country_code(df$publication_country, "country", "iso3c")

# ------------------------------------------------------------------

print("Add estimated paper consumption")
# One m2 is 100 * 100 cm2 = 1e4 cm2
# One km2 is 1000 * 1000 m2 = 1e6 m2 = 1e10 cm2
# Estimated average print run per document: 1000
printrun <- 1000
df.preprocessed <- mutate(df.preprocessed, paper.consumption.km2 = width * height * pagecount/2 * (1/1e10) * printrun)

# -------------------------------------------------------------------

print("Enrich author info")
# Life years + author_unique field
library(estc)
life.info <- read.csv(system.file("extdata/author_info.csv", package = "bibliographica"), stringsAsFactors = FALSE, sep = "\t")
ambiguous.authors <- bibliographica::ambiguous_authors_table()

# Combine synonymous authors; augment author life years where missing etc.
df.preprocessed <- augment_author(df.preprocessed, life.info, ambiguous.authors)

# -------------------------------------------------------------------

# TODO improve: many names are missing gender now
print("Estimate author genders")
# Assumes that the author name is in the form "Last, First".
df.preprocessed$author_gender <- get_gender(pick_firstname(df.preprocessed$author_name, format = "last, first"))

print("Self-published docs where author is known but publisher not")
# Note: also unknown authors are considered as self-publishers
print("Add a separate self-published field")
tmp <- self_published(df.preprocessed)
df.preprocessed$self_published <- tmp$self_published
df.preprocessed$publisher <- tmp$publisher

# -----------------------------

print("Calculate average page counts based on available data")
# TODO make a function that quickly returns this. No need to precalculate it.
print("Average pagecounts")
mean.pagecounts.multivol <- mean_pagecounts_multivol(df.preprocessed) 
mean.pagecounts.univol <- mean_pagecounts_univol(df.preprocessed) 
mean.pagecounts.issue <- mean_pagecounts_issue(df.preprocessed) 
mean.pagecounts <- full_join(mean.pagecounts.univol, mean.pagecounts.multivol, by = "doc.dimension")
mean.pagecounts <- full_join(mean.pagecounts, mean.pagecounts.issue, by = "doc.dimension")
mean.pagecounts$doc.dimension <- factor(mean.pagecounts$doc.dimension, levels = levels(mean.pagecounts.univol$doc.dimension))
# write.table(mean.pagecounts, file = paste(output.folder, "estimated_page_counts.csv", sep = ""), quote = F, row.names = F, sep = ",")

# -----------------------------------------------------------------------

print("Estimate total pages for the docs where it is missing")
df.preprocessed$pagecount.orig <- df.preprocessed$pagecount

# Gatherings 1to-4to (any number of vols) or >8to (with >10 vols)
# with missing page information are assumed to be 'issues'
# and we apply different estimated page count for them
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




