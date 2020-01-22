#' @title Approximate Years
#' @description Approximate year for entries that are approximations.
#' @param x year field (a vector)
#' @param polished_year Polished year information
#' @return data.frame with the fields 'start' and 'end'
#' @export
#' @author Leo Lahti and Hege Roivainen email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_years(c("1746", "1745-1750"))}
#' @keywords utilities
approximate_years <- function(x, polished_year = NULL) {

  x <- gsub("\\.*$", "", x)
  x <- gsub("\\[", "", x)
  x <- gsub("\\]", "", x)
  x <- gsub(" or later", "", x)
  

  x_parts <- strsplit(x, "-")


  # Get decade & century for each of the year parts
  x_part_mods <- lapply(x_parts, FUN = function(part) {
    
    if (length(grep("^[0-9]{3}[?]?$", part) > 0)) {
      decade <- paste0(substr(part,1,3), "0")
      century <- paste0(substr(part,1,2), "00")
    } else if (length(grep("^[0-9]{2}([?]{2})?$", part) > 0)) {
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
  
print(x_part_mods)  
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
  approx <- rep("year", length(x))
  inds <- grep("^[0-9]{3}[?]-*$", x)
  approx[inds] <- "decade"
  inds <- grep("^[0-9]{2}[?]{2}-*$", x)
  approx[inds] <- "century"   
  
  if (length(grep("^[0-9]*[?]-*$", x) > 0)) {
    x <- gsub("-", "0", x)
  }

  # Finally, discard years that have more than 4 characters
  x[nchar(x) > 4] <- NA

  data.frame(from = as.numeric(x), approximation = approx)

}