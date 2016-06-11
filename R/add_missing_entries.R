#' @title Add missing entries
#' @description Fill in missing entries in the data based on supplied information table
#' @param df data.frame with the fields given in the 'id' and 'field' arguments
#' @param info Information table including the 'id' and 'field' fields
#' @param id Field that is used to match 'df' and 'info' data.frames given as input
#' @param field Field that will be filled in 'df' based on the information in 'info' data.frame
#' @return Character vector of unique author IDs
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{a <- add_missing_entries(...)}
#' @keywords utilities
add_missing_entries <- function (df, info, id, field) {
  v <- df[[field]]
  inds <- which(is.na(v) & (df[[id]] %in% info[[id]]))
  v[inds] <- info[match(df[[id]][inds], info[[id]]), field]
  v
}

