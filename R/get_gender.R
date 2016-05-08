#' @title Get gender
#' @description Pick gender based on first names.
#' @param x Vector of first names
#' @param gendermap Table with name-gender mappings
#' @return Author gender information
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples
#'   \dontrun{
#'     data(gendermap)
#'     get_gender(c("armi", "julius"), gendermap)
#' }
#' @keywords utilities
#' @importFrom gender gender
#' @import genderdata
get_gender <- function (x, gendermap) {

  # polish up
  x <- tolower(condense_spaces(gsub("\\.", " ", as.character(x))))

  first.names.orig <- x
  first.names.uniq <- unique(na.omit(first.names.orig))
  first.names <- first.names.uniq
  gendermap$name <- tolower(gendermap$name)  

  # Only keep the names that are in our present data. Speeding up
  mynames <- unique(c(first.names, tolower(unlist(strsplit(first.names, " ")))))
  gendermap <- gendermap[gendermap$name %in% mynames,]

  # Custom gender mappings to resolve ambiguous cases
  # bibliographica::"extdata/names/firstnames/gender.csv",   
  # Consider the custom table as primary  
  # ie override other matchings with it
  custom <- gender_custom()
  if (any(custom$name %in% gendermap$name)) {
    inds <- match(custom$name, gendermap$name)
    inds2 <- which(!is.na(inds))
    gendermap[inds[inds2], "gender"] <- custom$gender[inds2]
  }
  
  # Also add new custom names to get the final table
  map <- rbind(gendermap, custom[!custom$name %in% gendermap$name,1:2])
  map <- unique(map)

  # None of our names are in the gender map
  # return NA for all
  if (nrow(map) == 0) {
    return(rep(NA, length(x)))
  }

  # Split by space
  spl <- strsplit(first.names, " ")
  len <- sapply(spl, length)

  # Match unique names to genders
  gender <- rep(NA, length(first.names))

  # First check cases with a unique name
  inds <- which(len == 1)
  gender[inds] <- harmonize_names(first.names[inds], map, from = "name", to = "gender", remove.unknown = TRUE)

  # Then cases with multiple names split by spaces
  # if different names give different genders, then set to NA
  inds <- which(len > 1)
  gtmp <- lapply(spl[inds], function (x) {unique(na.omit(harmonize_names(x, map, from = "name", to = "gender", remove.unknown = TRUE)))})
  # Handle ambiguous cases 
  gtmp[sapply(gtmp, length) == 0] <- NA
  gtmp[sapply(gtmp, length) > 1] <- "ambiguous"
  # Set the identified genders
  gtmp <- sapply(gtmp, identity)
  gender[inds] <- gtmp
  gender <- sapply(gender, identity)

  # Project unique names back to the original domain
  gender[match(first.names.orig, first.names.uniq)]
  
}



