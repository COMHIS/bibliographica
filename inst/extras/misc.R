  # Identify romans that are not in square brackets _and_ start the 
  # page count sequence (ie. "iii, iv, 2-10, 13-16" -> inds = 1:2)
  roman <- pagecount.attributes["roman", ]
  sqb <- pagecount.attributes["squarebracket", ]
  inds <- which(roman & !sqb)
  if (length(inds) == 1 && inds == 1) {   
  } else if (length(inds) > 1 && inds[[1]] == 1) {   
    inds <- 1:(which.min(diff(inds) == 1) - 1)
  } else {
    inds <- NULL
  }

  pagecount.attributes <- rbind(pagecount.attributes, roman.start = rep(FALSE, ncol(pagecount.attributes)))
  pagecount.attributes["roman.start", inds] <- TRUE


  # Romans at the start are counted separately
  inds <- pagecount.attributes["roman.start",]
  pages$roman.start <- maxrule(x[inds])
  # Set remaining romans FALSE if they are already listed in roman.start
  pagecount.attributes["roman", pagecount.attributes["roman.start",]] <- FALSE

