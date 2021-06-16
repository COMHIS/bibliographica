#' @title Get Gender
#' @description Pick gender based on first names.
#' @param x Vector of first names
#' @param gendermap Table with name-gender mappings
#' @return Author gender information
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples
#'   \dontrun{
#'     gendermap <- read_mapping(system.file("extdata/gendermap.csv",
#'       package = "bibliographica"), sep = "\t", from = "name", to = "gender")
#'     get_gender(c("armi", "julius"), gendermap)
#' }
#' @keywords utilities
get_gender <- function (x, gendermap) {

  # polish up
  x <- tolower(condense_spaces(gsub("\\.", " ", as.character(x))))

  first.names.orig <- x
  first.names.uniq <- unique(na.omit(first.names.orig))
  first.names <- first.names.uniq
  gendermap$name <- tolower(gendermap$name)  

  # Only keep the names that are in our present data. Speeding up
  mynames <- unique(c(first.names, unlist(strsplit(first.names, " "))))
  gendermap <- gendermap[gendermap$name %in% mynames,]
  map <- gendermap

  # None of our names are in the gender map
  # return NA for all
  if (nrow(map) == 0) {
    return(rep(NA, length(x)))
  }

  # Split by space
  spl <- strsplit(first.names, " ")
  len <- sapply(spl, length, USE.NAMES = FALSE)

  # Match unique names to genders
  gender <- rep(NA, length(first.names))

  # First check cases with a unique name
  inds <- which(len == 1)
  gender[inds] <- map(first.names[inds], map, from = "name", to = "gender", remove.unknown = TRUE)

  # Then cases with multiple names split by spaces
  # if different names give different genders, then set to NA
  inds <- which(len > 1)

  gtmp <- lapply(spl[inds], function (x) {unique(na.omit(map(x, map, from = "name", to = "gender", remove.unknown = TRUE)))})
  # Handle ambiguous cases
  len <- sapply(gtmp, length, USE.NAMES = FALSE)
  gtmp[len == 0] <- NA
  gtmp[len > 1] <- "ambiguous"
  # Set the identified genders
  gender[inds] <- sapply(gtmp, identity, USE.NAMES = FALSE)
  gender <- unname(sapply(gender, identity, USE.NAMES = FALSE))

  # Project unique names back to the original domain
  gender[match(first.names.orig, first.names.uniq)]
  
}



