#' @importFrom sorvi harmonize_names
harmonize_pages <- function (x, synonyms, harm) {

  # Remove dimension info
  x <- harmonize_names(x, synonyms, mode="match")$name  
  s <- remove_dimension(x)

  # ie harmonization
  # each comma place separately
  # each dash place separately  
  s <- gsub("e\\.\\,", "e ", s)

  s <- str_trim(unlist(strsplit(s, ",")))

  s <- sapply(s, function (si) {x <- unlist(strsplit(si, "-")); paste(sapply(x, function (x) handle_ie(x)), collapse = "-")})

  s <- paste(s, collapse = ",")

  # Romans
  s <- harmonize_romans(s) 
  s <- harmonize_names(s, harm, mode = "recursive")$name
  s <- condense_spaces(s)

  # Remove endings
  for (i in 1:5) {
    s <- str_trim(remove_endings(s, c(" ", "\\.", "\\,", "\\;", "\\:")))
  }

  # Harmonize sheet, plate and table info
  s <- harmonize_sheets(s)

  # Pp. -> p etc.
  s <- harmonize_page_info(s)

  # Remove spaces around dashes and parentheses
  s <- gsub(" -", "-", s)
  s <- gsub("- ", "-", s)
  s <- str_trim(gsub("\\)", " ", gsub("\\(", " ", s)))
  s <- condense_spaces(s)

  # Add commas
  # "[2] 4 p." -> "[2], 4 p."
  inds <- setdiff(1:length(s), grep("\\[i", s))
  s[inds] <- gsub(" \\[", "\\,[", s[inds])
  for (n in 0:9) {
    s <- gsub(paste("] ", n, sep = ""), paste("], ", n, sep = ""), s)
  }
  # Remove too many commas
  s <- gsub("\\,+", ",", s)

  # p3 -> 3
  if (length(grep("^p[0-9]", s))) {
    s <- substr(s, 2, nchar(s))
  }

  # [24 } -> 24
  if (length(grep("^[[({][0-9]*[])}]$", gsub(" ", "", s)))) {
    s <- gsub("\\[", "", s)
    s <- gsub("\\]", "", s)
    s <- gsub("\\{", "", s)
    s <- gsub("\\}", "", s)
  }

  s <- str_trim(gsub("^,", "", s))
  if (s %in% c("", "NA")) { s <- NA }

  s

}