#' @title validate_names
#' @description Validate names
#' @param namelist Vector of names to be validated
#' @param database Specify the name database to be used 
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
validate_names <- function (namelist, database) {

  # Get name lists from public databases
  # TODO: validating names could make its own R package later,
  # perhaps combined with the gender package?
  
  if (database == "last") {
    accepted_names <- tolower(as.character(lastnames()))
    not_names <- tolower(as.character(notnames()))
  } else if (database == "first") {
    accepted_names <- as.character(firstnames()$name)
    # Also accept individual letters for first names
    accepted_names <- tolower(unique(c(accepted_names, letters)))
    not_names <- tolower(as.character(notnames()))
  }

  # Many names have multiple parts
  # Split to components and check each component is among accepted names
  uniq.names <- unique(namelist)
  uniq.names.spl <- strsplit(uniq.names, " ")
  uniq.names.spl2 <- lapply(uniq.names.spl, function (x) {unlist(strsplit(x, "-"))})
  uniq.names.spl3 <- lapply(uniq.names.spl2, function (x) {str_trim(x)})

  # Accept names where all components (Jean-Luc -> Jean Luc for instance) are 
  # among accepted names
  ok <- sapply(uniq.names.spl3, function (x) {all(x %in% accepted_names)})

  validated <- namelist %in% unique(namelist)[ok]

  # Final accepted names
  # accepted <- unique(namelist[validated]) 

  # List name components that cannot be validated from public name databases
  # and not included in the custom stopword list
  sdif <- setdiff(unlist(uniq.names.spl3), c(not_names, accepted_names, "", "NA", NA))
  counts <- rev(sort(table(sdif)))
  invalid <- data.frame(list(Name = names(counts), Count = counts))

  list(validated = validated, invalid = invalid)

}

