
maxrule <- function (x) {

  xx <- as.numeric(x)
  if (any(is.na(xx))) {	
    xx <- na.omit(suppressWarnings(as.numeric(unlist(strsplit(unlist(strsplit(x, "-"), use.names = FALSE), " "), use.names = FALSE))))
  }

  max(xx)
  
}


intervalrule <- function (x, revert = FALSE) {

  xx <- na.omit(suppressWarnings(as.numeric(unlist(strsplit(x, "-"), use.names = FALSE))))

  # Revert the series around before calculation
  if (revert) {
    xx <- rev(xx)
  }
  
  max(xx) - min(xx) + 1

}
