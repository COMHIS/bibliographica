#' @title Guess Missing Entries
#' @description Fill in missing values. This function assumes that
# 'each unique id has a unique value which can be missing for some
#' entries of the id but available in others. The missing data will be
#' filled based on the available values. Ambiguous cases (same id but
#' multiple values) are ignored.
#' @param id identifier vector
#' @param values corresponding values with potentially missing information (NAs)
#' @return A vector with augmented values
#' @examples \dontrun{guess_missing_entries(
#'   id = c("Tom", "Tom", "Pete", "Pete", "Pete"),
#'   	                 values = c(1, NA, 2, 3, NA))}
#' @export
#' @details For instance, we may have authors and author life
#' years (birth and death). The life years may be available for
#' a given author in some entries and missing in others.
#' When the information is unique, this function fills the missing
#' entries where possible. The function checks that the available
#' life years for the given id (author in this example) are unique
#' (ie. to avoid ambiguous mappings, for instance distinct authors
#' with identical name but different birth year). For unique id-value
#' relation, use the unique value to fill in the missing entries for
#' the same id)
#' @keywords utils
guess_missing_entries <- function (id, values) {

  id <- as.character(id)
  values <- as.character(values)		        
  tab <- cbind(id = id, values = values)

  # Unique entries
  spl <- split(tab[, "values"], tab[, "id"])
  spl <- lapply(spl, function (x) {unique(na.omit(x))})
  uniq <- names(which(sapply(spl, function (x) {length(x)}) == 1))
  spl <- spl[uniq]

  naind <- is.na(values) & (id %in% uniq)

  if (length(naind)>0) {
    values[naind] <- unlist(spl[id[naind]], use.names = FALSE)
  }

  quickdf(list(id = id, values = values))
  #data.frame(id = id, values = values, stringsAsFactors = FALSE)
  
}

