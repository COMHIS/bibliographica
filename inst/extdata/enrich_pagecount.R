
enrich_pagecount <- function(df.preprocessed) {

  print("Add volume info where missing")
  
  if (!"volcount" %in% names(df.preprocessed)) {
    df.preprocessed$volcount <- rep(1, nrow(df.preprocessed))
  }

  if (!"volnumber" %in% names(df.preprocessed)) {
    df.preprocessed$volnumber <- rep(NA, nrow(df.preprocessed))
  }

  # --------------------------------------------------------------------------

  print("Estimate total pages for the docs where it is missing")
  df.preprocessed$pagecount.orig <- df.preprocessed$pagecount

  # ----------------------------------------------------------------------------

  # Recognize categories
  df.preprocessed$singlevol = is.singlevol(df.preprocessed)
  df.preprocessed$multivol = is.multivol(df.preprocessed)
  df.preprocessed$issue = is.issue(df.preprocessed)

  # --------------------------------------------------------------------------

  print("Calculate average page counts based on available data")
  # TODO make a function that quickly returns this. No need to precalculate it.
  source(system.file("extdata/mean_pagecounts.R", package = "bibliographica"))
  mean.pagecounts.results <- get_mean_pagecounts(df.preprocessed)

  mean.pagecounts.singlevol <- mean.pagecounts.results$singlevol
  mean.pagecounts.multivol  <- mean.pagecounts.results$multivol
  mean.pagecounts.issue     <- mean.pagecounts.results$issue

  write.table(mean.pagecounts.singlevol,
              file = "mean_pagecounts_singlevol.csv",
              sep = ",",
              quote = F,
              row.names = F)
  write.table(mean.pagecounts.multivol,
              file = "mean_pagecounts_multivol.csv",
              sep = ",",
              quote = F,
              row.names = F)
  write.table(mean.pagecounts.issue,
              file = "mean_pagecounts_issue.csv",
              sep = ",",
              quote = F,
              row.names = F)

  # --------------------------------------------------------------------------

  print("Identify issues that are missing pagecount")
  # and add page count estimates
  inds1 <- df.preprocessed$issue & is.na(df.preprocessed$pagecount)
  df.preprocessed[inds1, "pagecount"] <- estimate_pages_issue(df.preprocessed[inds1,],
                                                              mean.pagecounts.issue)

  # Identify multi-vol docs
  # .. and then take only those without page count
  # ... also consider docs with <10 pages having missing page info as
  # these are typically ones with only some plate page information and
  # missing real page information
  inds <- df.preprocessed$multivol & is.na(df.preprocessed$pagecount)
  inds2 <- inds
  df.preprocessed[inds, "pagecount"] <- estimate_pages_multivol(df.preprocessed[inds,],
                                                                mean.pagecounts.multivol)

  # Single-vol docs missing pagecount
  inds3 <- df.preprocessed$singlevol & is.na(df.preprocessed$pagecount)
  df.preprocessed[inds3, "pagecount"] <- estimate_pages_singlevol(df.preprocessed[inds3,],
                                                                  mean.pagecounts.singlevol)

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
