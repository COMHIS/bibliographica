#' @title Polish Publisher Generic
#' @description Generic cleanup of the publisher field.
#' @param x Character vector of publisher names
#' @return Data frame with orig, mod
#' @export
#' @author Leo Lahti \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @keywords utilities
polish_publisher <- function (x) {

  # Lowercase		 
  # x <- tolower(x)

  # .test -> test
  x <- gsub("^\\.*", "", x)

  # test. -> test
  x <- gsub("\\.*$", "", x)

  # s.n -> ""
  x <- gsub("^\\[*s\\.*n\\.*\\]*$", " ", x)

  x <- condense_spaces(x)
  x
}
