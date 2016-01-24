#' @importFrom sorvi harmonize_names
harmonize_pages <- function (s, synonyms, harm, sheetharm, harm.pi) {

  s <- unlist(strsplit(s, ","))
  s <- sapply(s, function (si) {x <- unlist(strsplit(si, "-")); paste(sapply(x, function (x) handle_ie(x, harmonize = FALSE)), collapse = "-")})
  s <- paste(s, collapse = ",")

  # FIXME move to main? pages
  s <- harmonize_names(s, harm, mode = "recursive")$name

  # FIXME move to main? page info
  s <- harmonize_names(s, harm.pi, mode = "recursive")$name

  # Remove endings
  for (i in 1:5) {
    s <- remove_endings(s, c(" ", "\\.", "\\,", "\\;", "\\:"))
  }

  # Remove spaces around dashes
  s <- gsub(" {0,1}- {0,1}", "-", s)

  # Remove parentheses
  s <- gsub("[\\(|\\)]", " ", s)
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
    s <- gsub("^p", "", s)
  }

  # [24 } -> 24
  if (length(grep("^[[({][0-9]*[])}]$", gsub(" ", "", s)))) {
    s <- gsub("[\\[|\\]|\\{|\\}]", "", s)
  }

  s <- gsub("^,", "", s)
  s <- str_trim(gsub("\\]$", "", s))  
  if (s %in% c("", "NA")) { s <- NA }

  s

}