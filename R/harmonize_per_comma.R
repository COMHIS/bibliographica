
harmonize_per_comma <- function (x) {

  # Split by comma and handle comma-separated elements as 
  # interpretation units
  spl <- str_trim(unlist(strsplit(x, ",")))

  # Harmonize pages within each comma
  x <- sapply(spl, function (x) { harmonize_pages_by_comma(x) })

  # Remove empty items
  x <- as.vector(na.omit(x))

  x

}