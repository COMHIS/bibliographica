pages2arabics <- function (s) {

  # Convert romans to arabics (entries separated by spaces possibly)
  spl <- str_trim(unlist(strsplit(s, " "), use.names = FALSE))

  ns <- c()
  romans <- rep(FALSE, length(spl))
  for (i in 1:length(spl)) {

    x <- spl[[i]]

    if (length(grep("-", x)) > 0) {
      x2 <- str_trim(unlist(strsplit(x, "-"), use.names = FALSE))
      n <- suppressWarnings(as.numeric(as.roman(x2)))
      n[is.na(n)] <- x2[is.na(n)] # vii-160
      if (any(is.roman(x2))) {
        romans[[i]] <- TRUE
      }
      x <- paste(n, collapse = "-")
    } else {
      if (is.roman(x)) {
        romans[[i]] <- TRUE
      }
      x <- suppressWarnings(as.numeric(as.roman(x)))
    }
  
    ns[[i]] <- x

  }

  nums <- !is.na(ns)

  if (sum(nums) > 0) {
    spl[nums] <- ns[nums]
    romans[nums] <- romans[nums]
    arabic <- spl[!romans]
    roman <- spl[romans]
  } else {
    arabic <- roman <- roman.logical <- NA
  }

  res <- list(converted.numbers = spl, roman = roman, arabic = arabic, roman.logical = romans)

  res
}

