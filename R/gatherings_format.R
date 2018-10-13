# Recognize gatherings format
gatherings_format <- function (x) {

  gtab <- gatherings_table()

  # Majority vote
  names(which.max(apply(gtab, 2, function (i) {sum(x %in% i)})))
  
}