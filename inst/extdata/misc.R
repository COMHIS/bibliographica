  # OK, now we have polished first and last names
  # To speed up, discard names where both first and last are not accepted
#  valid <- list()
#  #invalid <- list()
#  for (db in c("first", "last")) {
#
#    namelist <- nametab[[db]]
#    v <- list()
#    v$validated <- !is.na(namelist)
#    v$invalid <- suniq[is.na(namelist)]
#    valid[[db]] <- v$validated
#    #invalid[[db]] <- v$invalid
#
#  }
#  nametab[(!valid[["first"]] | !valid[["last"]]), ] <- NA
#  nametab$last[is.na(nametab$first)] <- NA
#  nametab$first[is.na(nametab$last)] <- NA
# 
#  # FIXME this could go to enrich / qualitycheck
#  ### VALIDATING THE NAMES
#  valid <- list()
#  #invalid <- list()
#  if (verbose) { message("Validate names with known name lists") }
#  if (validate) {  
#    for (db in c("first", "last")) {
#      if (verbose) { message(db) }
#      namelist <- nametab[[db]]
#      v <- validate_names(namelist, db)
#      valid[[db]] <- v$validated
#      #invalid[[db]] <- v$invalid
#    }
#    if (verbose) { message("Remove names that do not have both valid first and last names") }
#    nametab[(!valid[["first"]] | !valid[["last"]]), ] <- NA
#    nametab$last[is.na(nametab$first)] <- NA
#    nametab$first[is.na(nametab$last)] <- NA
#  }

