

all2arabics <- function (x) {

  # Convert all non-plate pages into arabics (arabics, romans, square
  # brackets, dashed) in the order of occurrence
  # Remove dashes
  xseries <- str_trim(unlist(strsplit(x, "-"), use.names = FALSE))

  # Remove square brackets
  xseries <- str_trim(gsub("\\]", " ", gsub("\\[", " ", xseries)))  

  # Convert to arabics
  xseries <- sapply(xseries, function (x) suppressWarnings(as.numeric(as.roman(x))), USE.NAMES = FALSE)

  xseries
  
}


