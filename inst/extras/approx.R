  # Get decade & century for each of the year parts
  x_part_mods <- lapply(x_parts, FUN = function(part) {
    
    if (length(grep("^[0-9]{3}-*[?]?$", part) > 0)) {
      decade <- paste0(substr(part,1,3), "0")
      century <- paste0(substr(part,1,2), "00")
    } else if (length(grep("^[0-9]{2}-*([?]{2})?$", part) > 0)) {
      decade <- NA
      century <- paste0(substr(part,1,2), "00")
    } else if (length(grep("^[0-9]{4}$", part) > 0)) {
      decade <- paste0(substr(part,1,3), "0")
      century <- paste0(substr(part,1,2), "00")
    } else {
      decade <- NA
      century <- NA
    }
    
	x_part_mods <- list(decade, century)
	
  })

  # If decade & century are NA try again with an already polished value
  # First row is for decades
  q <- sapply(x_part_mods, is.na)  
  inds <- which(q[1,])

  if (!is.null(polished_year)) {
    x_part_polished <- strsplit(polished_year[inds], "-")
  
    x_part_polished_mods <- lapply(x_part_polished, FUN = function(part) {
      if (length(grep("^[0-9]{4}$", part) > 0)) {
        decade <- paste0(substr(part,1,3), "0")
        century <- paste0(substr(part,1,2), "00")
      } else {
        decade <- NA
        century <- NA
      }

    list(decade, century)
    
  })
  
  x_part_mods[inds] <- x_part_polished_mods

  }

  # Check that decade & century are the same for each of the parts
 
  decades <- sapply(x_part_mods, FUN=function(decade) {
    ret_decade <- sapply(decade, function(dec) {
      if (length(dec) == 2) {
        if (dec[1] == dec[2]) {
          ret_decade <- dec[1]
        } else {
          ret_decade <- NA
        }
      } else if (length(dec) == 1) {
        ret_decade <- dec[1]
      } else {
        ret_decade <- NA
      }  
    })
    decades <- ret_decade
  })
