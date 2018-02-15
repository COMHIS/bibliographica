#' @title Map Terms
#' @description Map between two sets of terms; used to harmonize data.
#' @details Map the input vector to harmonized versions based on the synonyme
#'          table.
#' @param x A character vector 
#' @param synonymes synonyme table with the fields 'synonyme' and 'name'
#' @param remove.unknown Logical. Remove terms that do not have synonymes.
#' @param mode 'exact.match' replaces the terms based on the synonyme list
#'        if an exact match is found; 'match' replaces the parts that match
#'        synonymes; 'recursive' replaces all (sub)strings recursively in
#'        the same order as in the synonyme table
#' @param verbose verbose
#' @param from field that will be replaced
#' @param to field that contains the final names
#' @param keep.names Keep the original names in the returned vector
#'        (may slow down analysis in large data sets)
#' @return Vector of harmonized terms
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- map(x, synonymes)}
#' @keywords utilities
map <- function (x, synonymes, remove.unknown = FALSE, mode = "exact.match", verbose = FALSE, from = "synonyme", to = "name", keep.names = FALSE) {

  if (verbose) {message("Map synonymes to selected names: NA if mapping not available")}
  xorig <- as.character(x)
  xx <- xuniq <- unique(xorig)
  synonymes <- synonymes[, c(from, to)]
  colnames(synonymes) <- c("synonyme", "name")
  for (i in 1:ncol(synonymes)) {
    synonymes[,i] <- as.character(synonymes[,i])
  }

  if (verbose) {message(mode)}
  
  if (mode == "exact.match") {

    # By default each term maps to itself
    # TODO to speed up remove first those that match directly
    # Only check those cases that overlap
    #inds <- which(xuniq %in% synonymes$synonyme)
    # Hack due some odd R bug that renders the previous line unfunctional
    inds <- which(xuniq %in% intersect(synonymes$synonyme, xuniq))

    if (remove.unknown && length(inds) == 0) {
      xx <- rep(NA, length(xx))
    } else if (remove.unknown && length(inds) > 0 && length(inds) < length(xx)) {
      xx[-inds] <- NA
    }

    for (i in inds) {

        inds2 <- which(synonymes$synonyme == xuniq[[i]])
        xh <- unique(synonymes$name[inds2])

        if (length(xh) == 1) {
          xx[[i]] <- xh
        } else if (length(xh) > 1)  {
          warning(paste("No unique mapping for", xuniq[[i]]))
          xx[[i]] <- NA
        } else if (length(xh) == 0 && remove.unknown)  {
          xx[[i]] <- NA
        }
    }

  } else {

    if (mode == "match") {
      # Go through synonymes from longest to shortest
      synonymes <- synonymes[rev(order(nchar(synonymes[, "synonyme"]))),]
    }
    
    # mode: "match" and "recursive"
    for (i in 1:nrow(synonymes)) {

      from <- synonymes[i, "synonyme"]

      if (!is.null(from) && (length(from) > 0 ) && (!is.na(from))) {
      
      #if (verbose) {message("-------------")}      
      #if (verbose) {message(xx)}
      #if (verbose) {message(from)}
      
        inds <- grep(from, xx)
        if (length(inds) > 0) {

	  to <- synonymes[i, "name"]
          xx[inds] <- gsub(from, to, xx[inds])
	  
	  #print(synonymes[i,])
	  #print(unique(xx[inds]))
	  #print("---")

        }
	
      }
    }
  }
  

  # Map back to original inds 
  xx <- xx[match(xorig, xuniq)]

  if (keep.names) {
    names(xx) <- xorig
  }

  xx

}


