#' @title guess_missing_entries
#' @description Fill in the missing values. This function assumes that each id has a unique value which is missing for some occurrences of the id but available in others and copied from there to fill in the missing occurrences.
#' @param id identifier vector
#' @param values corresponding values with potentially missing information (NAs)
#' @return A vector with augmented values
#' @examples \dontrun{guess_missing_entries(id = c("Tom", "Tom", "Pete", "Pete", "Pete"), values = c(1, NA, 2, 3, NA))}
#' @export
#' @details For instance, we may have authors and author life years (birth and death).
#' The life years may be available for a given author in some entries and missing in others.
#' When the information is unique, this function fills the missing entries where possible.
#' The function checks that the available life years for the given id (author in this
#' example) are unique (ie. to avoid ambiguous mappings, for instance distinct authors with
#' identical name but different birth year). For unique id-value relation, use the unique
#' value to fill in the missing entries for the same id (
#' @keywords utils
guess_missing_entries <- function (id, values) {

  tab <- cbind(id = id, values = values)
  tab <- tab[!apply(is.na(tab), 1, any),]

  # Unique entries
  spl <- split(tab[, "values"], tab[, "id"])  
  uniq <- names(which(sapply(spl, length) == 1))
  spl <- spl[uniq]

  naind <- is.na(values) & (id %in% uniq)
  if (any(naind)) {
    values[naind] <- unlist(spl[id[naind]], use.names = FALSE)
  }

  data.frame(list(id = id, values = as.character(values)), stringsAsFactors = FALSE)
  
}

