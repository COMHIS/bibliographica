#' @title Annual Publication Frequency to Text
#' @description Convert annual publication frequencies to text format.
#' @param x publication frequency per year
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- publication_frequency_text("Once a year", 1)}
#' @keywords utilities
publication_frequency_text <- function (x, peryear) {

  peryear <- as.numeric(peryear)
  text <- as.character(x)
  text[!is.na(peryear)] <- as.character(peryear[!is.na(peryear)])

  text[round(peryear) == 365] <- "Daily"
  text[round(peryear) == 312] <- "Six per Week"    
  text[round(peryear) == 208] <- "Four per Week"  
  text[round(peryear) == 156] <- "Three per Week"  
  text[round(peryear) == 104] <- "Twice per Week"  
  text[round(peryear) == 52] <- "Weekly"
  text[round(peryear) == 36] <- "Three per Month"
  text[round(peryear) == 26] <- "Every two Weeks"  
  text[round(peryear) == 24] <- "Twice per Month"  
  text[round(peryear) %in% 15:18] <- "Every three Weeks"
  text[round(peryear) %in% 11:13] <- "Monthly"
  text[round(peryear) == 9] <- "Nine per Year"    
  text[round(peryear) == 8] <- "Eight per Year"
  text[round(peryear) == 7] <- "Seven per Year"
  text[round(peryear) == 6] <- "Every two months"
  text[round(peryear) == 5] <- "Five per Year"
  text[round(peryear) == 4] <- "Every three Months"
  text[round(peryear) == 3] <- "Every four Months"
  text[round(peryear) == 2] <- "Every six Months"
  text[as.character(round(peryear, 1)) %in% seq(0.8, 1.4, .1)] <- "Annual"
  text[as.character(round(peryear,1)) %in% .7] <- "Every One or Two Years"
  text[round(1/peryear) == 2] <- "Every two Years"  
  text[round(1/peryear) == 3] <- "Every three Years"
  text[round(1/peryear) == 4] <- "Every four Years"
  text[round(1/peryear) == 5] <- "Every five Years"
  text[round(1/peryear) == 6] <- "Every six Years"
  text[round(1/peryear) == 7] <- "Every seven Years"
  text[round(1/peryear) == 8] <- "Every eight Years"
  text[round(1/peryear) == 9] <- "Every nine Years"        
  text[round(1/peryear) == 10] <- "Every ten Years"
  text[round(1/peryear) >= 11] <- "Less than every ten Years"      

  # x[!is.na(text)] <- text[!is.na(text)]

  # Order the levels by frequency
  text <- factor(text, levels = unique(text[order(peryear)]))

  text
}


