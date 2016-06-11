#' @title Read Synonyme Table
#' @description Read synonyme table.
#' @param file Input file
#' @param mode The input file type: "list" or "table"; see details.
#' @param sep Separator mark.
#' @param self.match Include self matches
#' @param include.lowercase Include lowercase
#' @param ignore.empty Ignore the empty cases
#' @param sort Sort synonymes
#' @param verbose verbose
#' @param remove.ambiguous Remove ambiguous terms.
#' @param lowercase All synonymes considered in lowercase only
#' @param from field that will be replaced
#' @param to field that contains the final names
#' @return Synonyme data frame with the fields 'name' (the selected term) and 'synonyme' (the alternative terms).
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @details If mode = "list", each row of the input file corresponds to a unique entry with potentially multiple name variants, separated by semicolon. The first element gives the selected version of the name, the subsequent elements list synonymes that will be mapped to the selected version. If mode = "table", the file has two columns where each row corresponds to a unique entry and has the selected name and a single alternative name.
#' @examples \dontrun{syn <- read_synonymes(file)}
#' @keywords utilities
read_synonymes <- function (file, mode = "table", sep = ";", self.match = FALSE, include.lowercase = FALSE, ignore.empty = FALSE, sort = FALSE, verbose = FALSE, remove.ambiguous = TRUE, lowercase = FALSE, from = "synonyme", to = "name") {

  .Deprecated("read_mapping")

  # TODO sort by desired field

  if (mode == "list") {

    rf <- readLines(file)

    aa <- lapply(rf, function (x) {unique(unlist(strsplit(x, sep)))})
    names(aa) <- sapply(rf, function (x) {unlist(strsplit(x, sep))[[1]]}, USE.NAMES = FALSE)
  
    map <- NULL
    for (nam in names(aa)) {
      map <- rbind(map, cbind(rep(nam, length(aa[[nam]])), aa[[nam]]))
    }
    aa <- as.data.frame(map, stringAsFactors = FALSE)
    names(aa) <- c("name", "synonyme")
    
    aa$name <- as.character(aa$name)
    aa$synonyme <- as.character(aa$synonyme)
    aa <- aa[!duplicated(aa),]

  } else if (mode == "table") {
    aa <- read.csv(file, sep = sep, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    # aa <- fread(file, sep = sep, header = TRUE)    
    # Temporarily name columns as name and synonyme
    # (needed in check_synonymes)
    aa <- aa[, c(from, to)]
    colnames(aa) <- c("synonyme", "name")
  }

  if (lowercase) {
    aa[[from]] <- tolower(aa[[from]])
  }

  # Polish the synonyme table
  aa <- check_synonymes(aa, include.lowercase = include.lowercase, verbose = verbose, sort = sort, self = self.match, ignore.empty = ignore.empty, remove.ambiguous = remove.ambiguous)

  # Return original field names
  colnames(aa) <- gsub("name", to, colnames(aa))
  colnames(aa) <- gsub("synonyme", from, colnames(aa))    

  aa 

}

