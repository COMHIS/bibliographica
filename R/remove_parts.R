remove_parts <- function (x) {

  s <- as.character(x)

  # Remove parts
  # "27 parts" -> " "

  parts <- c("parts", "part", "pts")
  for (n in parts) {
    s <- gsub(paste("[0-9][0-9][0-9] ", n, " in", sep = ""), " ", s)
    s <- gsub(paste("[0-9][0-9] ", n, " in", sep = ""), " ", s)
    s <- gsub(paste("[0-9] ", n, " in", sep = ""), " ", s)

    s <- gsub(paste("[0-9][0-9][0-9] ", n, sep = ""), " ", s)
    s <- gsub(paste("[0-9][0-9] ", n, sep = ""), " ", s)
    s <- gsub(paste("[0-9] ", n, sep = ""), " ", s)

    s <- gsub(paste("in [0-9][0-9][0-9] ", n, sep = ""), " ", s)
    s <- gsub(paste("in [0-9][0-9] ", n, sep = ""), " ", s)
    s <- gsub(paste("in [0-9] ", n, sep = ""), " ", s)

    s <- gsub(paste("in [0-9][0-9][0-9]", n, sep = ""), " ", s)
    s <- gsub(paste("in [0-9][0-9]", n, sep = ""), " ", s)
    s <- gsub(paste("in [0-9]", n, sep = ""), " ", s)

  }

  s <- gsub(" in [0-9][0-9][0-9] ", " ", s)
  s <- gsub(" in [0-9][0-9] ", " ", s)
  s <- gsub(" in [0-9] ", " ", s)
  
  s <- condense_spaces(s)  

  s
}



