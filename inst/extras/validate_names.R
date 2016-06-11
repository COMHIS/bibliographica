#' @title Validate names
#' @description Validate names by known name lists.
#' @param namelist Vector of names to be validated
#' @param database Specify the name database to be used
#' @param verbose verbose
#' @return List with following elements:
#'    \itemize{
#'      \item validated Logical vector indicating the valid names
#'	\item invalid List of invalid name components
#'    }
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- validate_names(namelist, database)}
#' @keywords utilities
validate_names <- function (namelist, database, verbose = TRUE) {

  # Get name lists from public databases
  # TODO: validating names could make its own R package later,
  # perhaps combined with the gender package?
  namelist <- tolower(as.character(namelist))

  if (database == "last") {
    accepted_names <- tolower(as.character(lastnames()))
    not_names <- tolower(as.character(notnames()))
  } else if (database == "first") {

    stop("TODO")
    # accepted_names <- as.character(firstnames()$name)

    # Also accept individual letters for first names
    accepted_names <- tolower(unique(c(accepted_names, letters)))
    not_names <- tolower(as.character(notnames()))
  } else if (database == "full") {

    x <- namelist
    xorig <- as.character(x)
    x <- xuniq <- unique(x)
  
    ### VALIDATING THE NAMES
    nametab <- list()
    nametab[["last"]] <- sapply(strsplit(x, "\\, "), function (x) {x[[1]]})
    nametab[["first"]] <- sapply(strsplit(x, "\\, "), function (x) {ifelse(length(x) == 2, x[[2]], NA)})  

    valid <- list()
    for (db in c("first", "last")) {
      if (verbose) { message(db) }
      valid[[db]] <- validate_names(nametab[[db]], db)$validated
    }

    if (verbose) { message("Remove names that do not have valid first or last names") }
    validated <- (valid[["first"]] | valid[["last"]])

    if (verbose) { message("Map to the original indices") }
    validated <- validated[match(xorig, xuniq)]

    return(list(valid = validated, invalid.first = names(rev(table(nametab$first[!valid[["first"]]]))), invalid.last = names(rev(table(nametab$last[!valid[["last"]]])))))
     
  }

  # Many names have multiple parts
  # Split to components and check each component is among accepted names
  uniq.names <- unique(namelist)
  uniq.names.spl3 <- lapply(strsplit(uniq.names, " "), function (x) {str_trim(unlist(strsplit(x, "-"), use.names = FALSE))})

  # Accept names where all components (Jean-Luc -> Jean Luc for instance) are 
  # among accepted names
  ok <- sapply(uniq.names.spl3, function (x) {all(x %in% accepted_names)})
  validated <- (namelist %in% unique(namelist)[ok]) & !is.na(namelist)

  # List name components that cannot be validated from public name databases
  # and not included in the custom stopword list
  sdif <- setdiff(unlist(uniq.names.spl3, use.names = FALSE), c(not_names, accepted_names, "", "NA", NA))

  # Map back to the original domain
  #validated <- validated[match(namelist.orig, namelist.uniq)]
  #sdif <- sdif[match(namelist.orig, namelist.uniq)]  

  # FIXME why not just return counts as such? Would be faster
  counts <- rev(sort(table(namelist[!validated])))
  invalid <- quickdf(list(Name = names(counts), Count = counts))

  list(validated = validated, invalid = invalid)

}

