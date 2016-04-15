#' @importFrom sorvi harmonize_names
harmonize_pages <- function (s, synonyms, sheetharm) {

  # FIXME move to main? pages
  # s <- as.character(harmonize_names(s, harm, mode = "recursive"))

  # FIXME move to main? page info
  # s <- as.character(harmonize_names(s, harm.pi, mode = "recursive"))

  if (length(grep("i\\.e", s)) > 0) {
    s <- unlist(strsplit(s, ","), use.names = FALSE)

    s <- sapply(s, function (si) {x <- unlist(strsplit(si, "-"), use.names = FALSE); paste(sapply(x, function (x) handle_ie(x, harmonize = FALSE)), collapse = "-")})
    s <- paste(s, collapse = ",")
  }

  # Remove endings
  s <- gsub("[ |\\.|\\,|\\;|\\:]+$", "", s)

  # Remove spaces around dashes
  s <- gsub(" {0,1}- {0,1}", "-", s)

  # Remove parentheses
  s <- gsub("[\\(|\\)]", " ", s)
  s <- gsub("[\\{|\\}]", " ", s)  
  s <- condense_spaces(s)

  # p3 -> 3
  if (length(grep("^p[0-9]+", s))>0) {
    s <- gsub("^p", "", s)
  }

  # Add commas
  # "[2] 4 p." -> "[2], 4 p."
  inds <- setdiff(1:length(s), grep("\\[i", s))
  s[inds] <- gsub(" \\[", "\\,[", s[inds])
  for (n in 0:9) {
     s <- gsub(paste("] ", n, sep = ""), paste("], ", n, sep = ""), s)
  }
  s <- str_trim(gsub("\\]$", "", s))  

  s

}

