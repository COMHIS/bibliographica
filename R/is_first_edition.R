#' @title First Edition Identification
#' @description Identify potential first editions
#' @details Identifies unique title-author combinations and marks the earliest occurrence as first edition.
#' @param df data.frame with the fields: author, title, publication_year
#' @return Logical vector indicating the potential first editions
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{fed <- is_first_edition(df)}
#' @keywords utilities
is_first_edition <- function (df) {

  # Author-title combo		 
  id <- apply(df[, c("author", "title")], 1, function (x) {paste(x, collapse = "|")});

  # Split years and indices
  spl1 <- split(df$publication_year, id)
  spl2 <- split(df$original_row, id)

  # Cases with multiple years
  inds <- which(sapply(spl1, function (x) {sum(!is.na(x))}) > 1)

  # Identify row indices that correspond to minimum publication years
  # in unique author-title combos
  first <- c()
  later <- c()  
  for (ind in inds) {
    i <- !is.na(spl1[[ind]])
    mininds <- which(spl1[[ind]][i] == min(spl1[[ind]][i]))
    first <- c(first, spl2[[ind]][i][mininds])
    later <- c(later, spl2[[ind]][i][-mininds])
  }

  # Cases with a single occurrence are listed as NA
  # (as it is not sure if there are multiple editions)
  fed <- rep(NA, nrow(df))
  fed[df$original_row %in% unlist(first, use.names = FALSE)] <- TRUE
  fed[df$original_row %in% unlist(later, use.names = FALSE)] <- FALSE
  
  fed

}
