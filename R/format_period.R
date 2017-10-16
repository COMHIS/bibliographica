#' @title Format Time Periods
#' @description Conversions between time period formats.
#' @param x A vector (numeric/character/factor)
#' @return Converted entries.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples format_period("[1700, 1800)")
#' @details Converts format [1700, 1800) into 1700-1799. Used in polishing tables and figures.
#' @keywords utilities
format_period <- function (x) {

  xorig <- x
  x <- strsplit(as.character(x), "\\,")

  x1 <- sapply(x, function (xi) {xi[[1]]})
  x1 <- gsub("^[(|\\[]", "", x1)
  open <- grep("^\\(", sapply(x, function (xi) {xi[[1]]}))
  x1 <- as.numeric(x1)
  if (length(open) > 0) {x1[open] <- x1[open] + 1}

  x2 <- sapply(x, function (xi) {ifelse(length(xi) == 2, xi[[2]], NA)})
  x2 <- gsub("\\)$", "", x2)
  x2 <- gsub("\\]$", "", x2)  
  open <- grep("\\)$", sapply(x, function (xi) {ifelse(length(xi) == 2, xi[[2]], NA)}))
  x2 <- as.numeric(x2)  
  if (length(open) > 0) {x2[open] <- x2[open] - 1}

  y <- apply(cbind(x1, x2), 1, function (x) {ifelse (x[[1]] == x[[2]], x[[1]], paste(x, collapse = "-"))})
  y <- gsub("NA-NA", NA, y)
  y <- gsub("- *", "-", y)  

  if (is.factor(xorig)) {
    # Keep the same ordering for the levels as in the input
    conv <- unique(cbind(as.character(xorig), y))  
    levels <- conv[match(levels(xorig), conv[,1]),2]
    y <- factor(y, levels = levels)
  }
  if (is.numeric(xorig)) {y <- as.numeric(y)}  

  y
  
}