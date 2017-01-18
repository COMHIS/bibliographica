#' @title Annual Publication Frequency to Text
#' @description Convert annual publication frequencies to text format.
#' @param x Original publication frequency text
#' @param peryear Estimated annual publication frequency
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- publication_frequency_text("Twice per year" 2)}
#' @keywords utilities
publication_frequency_text <- function (x, peryear) {

  peryear <- as.numeric(peryear)
  peryear.text <- as.character(x)
  peryear.text[!is.na(peryear)] <- as.character(peryear[!is.na(peryear)])

  # TODO move to inst/extdata conversion table
  f <- system.file("extdata/frequency_conversions.csv", package = "bibliographica")
  freqs <- read_mapping(f, sep = ",", mode = "vector", include.lowercase = FALSE, from = "name", to = "annual")
  freqs <- sapply(freqs, function (s) {eval(parse(text=s))})

  # Match numeric frequencies to the closest option
  inds <- which(!is.na(peryear) & is.numeric(peryear))
  if (length(inds) > 0){
    nams <- sapply(peryear[inds], function (y) {names(which.min(abs(freqs - y)))})
    peryear.text[inds] <- nams
  }
  peryear.text[which(peryear < 0.1)] <- "Less than every ten Years"

  peryear.text <- condense_spaces(peryear.text)

  # Order the levels by frequency
  peryear.text <- factor(peryear.text, levels = unique(peryear.text[order(peryear)]))

  peryear.text
}


