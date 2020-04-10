#' @title Read Mapping
#' @description Read mapping table.
#' @param file Input file
#' @param mode The input file type: "list", "vector" or "table"; see details.
#' @param sep Separator mark.
#' @param self.match Include self matches
#' @param include.lowercase Include lowercase
#' @param ignore.empty Ignore the empty cases
#' @param sort Sort synonymes
#' @param verbose verbose
#' @param remove.ambiguous Remove ambiguous terms.
#' @param lowercase.only All synonymes considered in lowercase only
#' @param from field that will be replaced
#' @param to field that contains the final names
#' @param fast Use the fast method fread (sensitive to problems in table format)
#' @param encoding Character encoding (needed in Windows environment)
#' @param trim trim empty spaces from the beginning and end of the strings
#' @param empty.is.na Interpret empty conversions as NA. By default this is FALSE.
#' @return Synonyme data frame with the fields 'name' (the selected term) and 'synonyme' (the alternative terms).
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @details If mode = "list", each row of the input file corresponds to a unique entry with potentially multiple name variants, separated by semicolon. The first element gives the selected version of the name, the subsequent elements list synonymes that will be mapped to the selected version. If mode = "table", the file has two columns where each row corresponds to a unique entry and has the selected name and a single alternative name.
#' @examples \dontrun{syn <- read_mapping(file)}
#' @keywords utilities
read_mapping <- function (file, mode = "table", sep = ";", self.match = FALSE, include.lowercase = FALSE, ignore.empty = FALSE, sort = FALSE, verbose = FALSE, remove.ambiguous = TRUE, lowercase.only = FALSE, from = "synonyme", to = "name", fast = FALSE, encoding="UTF-8", trim = FALSE, empty.is.na = FALSE) {
  
  # TODO sort by desired field

  if (mode == "list") {
    
    rf <- suppressWarnings(readLines(file, encoding=encoding))
    
    aa <- lapply(rf, function (x) {unique(unlist(strsplit(x, sep), use.names = FALSE))})
    names(aa) <- sapply(rf, function (x) {unlist(strsplit(x, sep))[[1]]})
    
    map <- NULL
    for (nam in names(aa)) {
      map <- rbind(map, cbind(rep(nam, length(aa[[nam]])), aa[[nam]]))
    }
    aa <- as.data.frame(map, stringAsFactors = FALSE)
    names(aa) <- c("name", "synonyme")
    
    aa$name <- as.character(aa$name)
    aa$synonyme <- as.character(aa$synonyme)
    aa <- aa[!duplicated(aa),]
    
  } else if (mode %in% c("table", "vector")) {
  
    # aa <- read.csv(file, sep = sep, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

    if (fast) {
      aa <- suppressWarnings(fread(file, sep = sep, header = TRUE, encoding=encoding))
      aa <- as.data.frame(aa)

    } else {
		# hegroiva 20191108: added the quote="" in order to read values containing quotes
      aa <- read.csv(file, sep = sep, header = TRUE, encoding = encoding, quote="")
    }

    # Temporarily name columns as name and synonyme
    # (needed in check_synonymes)
    aa <- aa[, c(from, to)]
    colnames(aa) <- c("synonyme", "name")
    
  }
  
  if (lowercase.only) {
    aa[[from]] <- tolower(aa[[from]])
  }

  # Polish the synonyme table
  aa <- suppressWarnings(check_synonymes(aa, include.lowercase = include.lowercase, verbose = verbose, sort = sort, self = self.match, ignore.empty = ignore.empty, remove.ambiguous = remove.ambiguous))

  # Return original field names
  colnames(aa) <- gsub("name", to, colnames(aa))
  colnames(aa) <- gsub("synonyme", from, colnames(aa))    

  # Trim empty spaces from the end
  if (trim) {
    aa$synonyme = str_trim(aa$synonyme)
    aa$name = str_trim(aa$name)    
  }
  
  # Trim empty spaces from the end
  if (!empty.is.na) {
    for (field in names(aa)) {
      aa[[field]][is.na(aa[[field]])] <- ""
    }
  }    
  if (trim) {
    aa$synonyme = str_trim(aa$synonyme)
    aa$name = str_trim(aa$name)    
  }

  if (mode == "vector") {
    vec <- as.vector(aa[, to])
    names(vec) <- as.character(aa[, from])
    aa <- vec    
  }

  aa 
  
}
