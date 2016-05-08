#' @title Map terms
#' @description Map between two sets of terms; used to harmonize data.
#' @details Map the input vector to harmonized versions based on the synonyme table.
#' @param x A character vector 
#' @param synonymes synonyme table with the fields 'synonyme' and 'name'
#' @param remove.unknown Logical. Remove terms that do not have synonymes.
#' @param mode 'exact.match' replaces the terms based on the synonyme list if an exact match is  found; 'match' replaces the parts that match synonymes; 'recursive' replaces all (sub)strings recursively in the same order as in the synonyme table
#' @param verbose verbose
#' @param from field that will be replaced
#' @param to field that contains the final names
#' @return Vector of harmonized terms
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- map(x, synonymes)}
#' @aliases harmonize_names
#' @keywords utilities
map <- function (x, synonymes, remove.unknown = FALSE, mode = "exact.match", verbose = FALSE, from = "synonyme", to = "name") {

  harmonize_names(x, synonymes, remove.unknown, mode, verbose, from, to)

}

#' @title Harmonize names
#' @description Harmonize names
#' @details Map the input vector to harmonized versions based on the synonyme table.
#' @param x A character vector 
#' @param synonymes synonyme table with the fields 'synonyme' and 'name'
#' @param remove.unknown Logical. Remove terms that do not have synonymes.
#' @param mode 'exact.match' replaces the terms based on the synonyme list if an exact match is  found; 'match' replaces the parts that match synonymes; 'recursive' replaces all (sub)strings recursively in the same order as in the synonyme table
#' @param verbose verbose
#' @param from field that will be replaced
#' @param to field that contains the final names
#' @return Vector of harmonized terms
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- map(x, synonymes)}
#' @keywords utilities
harmonize_names <- function (x, synonymes, remove.unknown = FALSE, mode = "exact.match", verbose = FALSE, from = "synonyme", to = "name") {

  .Deprecated(map, msg = "Use the map function instead.")

  x <- as.character(x)

  # Map synonymes to selected names: NA if mapping not available
  xorig <- x
  xuniq <- unique(x)

  synonymes <- synonymes[, c(from, to)]
  colnames(synonymes) <- c("synonyme", "name")

  if (mode == "exact.match") {

    # By default each term maps to itself
    # TODO to speed up remove first those that match directly
    xx <- xuniq

    # Only check those cases that overlap
    inds <- which(xuniq %in% synonymes$synonyme)

    if (remove.unknown && length(inds) == 0) {
      xx <- rep(NA, length(xx))
    } else if (remove.unknown && length(inds) > 0 && length(inds) < length(xx)) {
      xx[-inds] <- NA
    }

    for (i in inds) {

        inds2 <- which(synonymes$synonyme == xuniq[[i]])
        xh <- unique(as.character(synonymes$name[inds2]))

        if (length(xh) == 1) {
          xx[[i]] <- xh
        } else if (length(xh) > 1)  {
          warning(paste("No unique synonyme mapping available for", xuniq[[i]]))
          xx[[i]] <- NA
        } else if (length(xh) == 0 && remove.unknown)  {
          xx[[i]] <- NA
        }
    }

    xx2 <- xx[match(xorig, xuniq)]

  } else if (mode == "match") {

    xx <- xuniq
    
    # Go through synonymes from longest to shortest
    synonymes <- synonymes[rev(order(nchar(as.character(synonymes[, "synonyme"])))),]

    for (i in 1:nrow(synonymes)) {

      xx <- gsub(synonymes[i, "synonyme"], synonymes[i, "name"], xx)
      # xx <- condense_spaces(xx)

    }
    xx2 <- xx[match(xorig, xuniq)]

  } else if (mode == "recursive") {

    xx <- xuniq
    for (i in 1:nrow(synonymes)) {
      xx <- gsub(synonymes[i, "synonyme"], synonymes[i, "name"], xx)
      # xx <- condense_spaces(xx)
    }
    xx2 <- xx[match(xorig, xuniq)]
    
  }

  as.character(xx2)

}


