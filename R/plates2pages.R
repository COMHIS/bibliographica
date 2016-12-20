plates2pages <- function (s) {

  plate.multiplier <- 2	     

  # If not plate number given, consider it as a single plate
  # Convert plates to pages: 1 plate = 2 pages
  if (length(grep("plate", s)) > 0 || length(grep("lea", s)) > 0) {

    # Remove square brackets
    s <- condense_spaces(gsub("\\[", " ", gsub("\\]", " ", s)))

    if (s == "plate") {
      s <- 1
    } else if (length(grep("\\[*[0-9+]\\]* p of plates", s)) > 0) {
      # "[16] p of plates" -> [16]
      s <- gsub(" p of plates", "", s)
      plate.multiplier <- 1
    } else if (length(grep("plates*", s) > 0) && length(grep("lea", s)) == 0) {
      # "plates" instances without "leaf" or "leaves"
      xi <- str_trim(gsub("plates*", "", s))
      xi <- gsub("\\]", "", gsub("\\[", "", xi))
      # When no plate number is given, use plates = 2 plates
      xi[xi == ""] <- 2
      s <- suppressWarnings(as.numeric(as.roman(xi)))
    } else if (length(grep("plate", s) > 0) && length(grep("lea", s)) == 0) {
      # "plate" instances without "leaf" or "leaves"
      xi <- str_trim(gsub("plate", "", s))
      xi <- gsub("\\]", "", gsub("\\[", "", xi))
      # When no plate number is given, use 1 (plate = 1 page)
      xi[xi == ""] <- 1
      s <- as.numeric(xi)
    } else if (length(grep("leaf", s)) > 0) {
      # "leaf" instances 
      xi <- str_trim(gsub("leaf", "", s))
      xi <- gsub("\\]", "", gsub("\\[", "", xi))
      xi[xi == ""] <- 1
      # When no leaf number is given, use 1 (1 leaf)
      # and multiply the numbers by 2 (1 leaf = 2 pages)
      s <- 2 * as.numeric(xi)
    } else if (length(grep("leaves{0,1}", s)) > 0) {
      # "leaves" instances 
      xi <- str_trim(gsub("leaves{0,1}", "", s))   
      xi <- gsub("\\]", "", gsub("\\[", "", xi))
      # When no leaf number is given, use 2 (2 leaves)
      xi[xi == ""] <- 2
      s <- as.numeric(as.roman(xi))
    }

    # multiply the numbers xi by 2 (4 leaves = 8 pages)
    s <- plate.multiplier * as.numeric(s)
    s <- paste(s, "pages calculated from plates")

  }

  s

}
