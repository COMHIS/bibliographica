# Identify multi-vol docs
multivol <- is.multivol(df)

# .. and then take only those without page count
# ... also consider docs with <10 pages having missing page info as
# these are typically ones with only some plate page information and
# missing real page information
inds <- multivol & (is.na(df$pagecount) | df$pagecount <= 10)
df.preprocessed[inds, "pagecount"] <- estimate_pages_multivol(df.preprocessed[inds,])

