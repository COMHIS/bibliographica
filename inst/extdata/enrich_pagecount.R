
enrich_pagecount <- function(df.preprocessed) {

  message("Add volume info where missing")
  gc()

  # If volcount field does not exist, then assume volcount 1 for
  # all documents. This is for compatibility reasons.
  # TODO: later fix downstream functions so that they can manage
  # without such artificially added volcounts
  if (!"volcount" %in% names(df.preprocessed)) {
    df.preprocessed$volcount <- rep(1, nrow(df.preprocessed))
  }

  # Same for volnumber
  if (!"volnumber" %in% names(df.preprocessed)) {
    df.preprocessed$volnumber <- rep(NA, nrow(df.preprocessed))
  }

  # --------------------------------------------------------------------------

  message("Estimate total pages for the docs where it is missing")
  df.preprocessed$pagecount.orig <- df.preprocessed$pagecount

  # ----------------------------------------------------------------------------

  # Recognize categories
  df.preprocessed$singlevol <- is.singlevol(df.preprocessed)
  df.preprocessed$multivol  <- is.multivol(df.preprocessed)
  df.preprocessed$issue     <- is.issue(df.preprocessed)

  # --------------------------------------------------------------------------

  message("Calculate average page counts based on available data")
  mean.pagecounts.results <- get_mean_pagecounts(df.preprocessed)

  message("..write into file")
  write.table(mean.pagecounts.results$singlevol,
              file = "mean_pagecounts_singlevol.csv",
              sep = ",",
              quote = F,
              row.names = F)
  write.table(mean.pagecounts.results$multivol,
              file = "mean_pagecounts_multivol.csv",
              sep = ",",
              quote = F,
              row.names = F)
  write.table( mean.pagecounts.results$issue,
              file = "mean_pagecounts_issue.csv",
              sep = ",",
              quote = F,
              row.names = F)

  # --------------------------------------------------------------------------

  message("Identify issues that are missing pagecount")
  # and add page count estimates
  inds1 <- df.preprocessed$issue & is.na(df.preprocessed$pagecount)
  df.preprocessed[inds1, "pagecount"] <- estimate_pages_issue(df.preprocessed[inds1,],
                                                               mean.pagecounts.results$issue)

  # Identify multi-vol docs
  # .. and then take only those without page count
  # ... also consider docs with <10 pages having missing page info as
  # these are typically ones with only some plate page information and
  # missing real page information
  inds <- df.preprocessed$multivol & is.na(df.preprocessed$pagecount)
  inds2 <- inds
  df.preprocessed[inds, "pagecount"] <- estimate_pages_multivol(df.preprocessed[inds,],
                                                                mean.pagecounts.results$multivol)

  # Single-vol docs missing pagecount
  inds3 <- df.preprocessed$singlevol & is.na(df.preprocessed$pagecount)
  df.preprocessed[inds3, "pagecount"] <- estimate_pages_singlevol(df.preprocessed[inds3,],
                                                                  mean.pagecounts.results$singlevol)

  # Store information on cases where pages were estimated
  estimated.pagecount <- cbind(id = df.preprocessed$original_row,
  		       	     issue = inds1, multivol = inds2, singlevol = inds3)

  # -----------------------------------------------

  # 1to pitäisi aina olla tasan 2 sivua.
  # Eli yksi sheet, broardside tai 1to (kutsutaan millä tahansa nimellä),
  # mutta siinä on aina yksi lehti (ja siten kaksi sivua).
  # Näin ollen kaikki merkinnät joissa >2 sivua voisi siirtää 2fo kategoriaan.
  df.preprocessed[which(df.preprocessed$gatherings == "1to" &
                        df.preprocessed$pagecount > 2), "gatherings"] <- "2fo"

  return (df.preprocessed)
}
