#' @title Remove Time Info
#' @description Remove time information.
#' @param x Vector (time field)
#' @param verbose verbose
#' @param months months to remove
#' @return Polished vector
#' @export
#' @details Remove months, year terms and numerics
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- remove_time_info(x)}
#' @keywords utilities
remove_time_info <- function (x, verbose = FALSE, months = NULL) {

  if (is.null(months)) {
    f <- system.file("extdata/months.csv", package = "bibliographica")
    months <- as.character(read.csv(f, header = TRUE)[,1])
    months <- unique(c(months, tolower(months)))

    # Handle from longest to shortest to avoid problems
    months <- months[rev(order(nchar(months)))]
  }

  # 17th century 
  x <- condense_spaces(gsub("[0-9]*th century", " ", x))

  # July 8
  for (month in months) {

    # "march-1777"
    s <- paste0(month, "-")
    if (verbose) { message(paste("Removing", s)) }
    x <- gsub(s, " ", x)    

    # "17 or 18 February"
    s <- paste("[0-9]{1,2} or [0-9]{1,2} ", month, sep = "")
    if (verbose) {message(paste("Removing", s))}    
    x <- gsub(s, " ", x)    

    # " 17 February"
    s <- paste(" [0-9]{1,2} ", month, sep = "")
    if (verbose) {message(paste("Removing", s))}    
    x <- gsub(s, " ", x)

    # "[17 February"
    s <- paste("\\[[0-9]{1,2} ", month, sep = "")
    if (verbose) {message(paste("Removing", s))}    
    x <- gsub(s, " ", x)

    # "^17 February"
    s <- paste("^[0-9]{1,2} ", month, sep = "")
    if (verbose) {message(paste("Removing", s))}    
    x <- gsub(s, " ", x)

    s <- paste(month, " [0-9]{1,2} ", sep = "")
    s2 <- paste(month, " [0-9]{1,2}\\]", sep = "")    
    s3 <- paste(month, " [0-9]{1,2}$", sep = "")
    s4 <- paste(month, " [0-9]{1,2}\\,", sep = "")
    s5 <- paste(month, "\\, [0-9]{1,2}", sep = "")            
    if (verbose) {message(paste("Removing", s))}
    x <- gsub(s, " ", x)
    if (verbose) {message(paste("Removing", s2))}    
    x <- gsub(s2, " ", x)
    if (verbose) {message(paste("Removing", s3))}    
    x <- gsub(s3, " ", x)
    if (verbose) {message(paste("Removing", s4))}    
    x <- gsub(s4, " ", x)
    if (verbose) {message(paste("Removing", s5))}    
    x <- gsub(s5, " ", x)                

  }

  # other time information
  terms <- c("Anno\\.", "An\\. Do\\.", "year", "anno dom", "anno")
  toremove <- c(months, terms)

  x <- remove_terms(x, toremove)

  x

}

