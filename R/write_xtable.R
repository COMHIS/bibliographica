#' @title write_xtable
#' @description Write xtable in a file
#' @param x a vector or matrix
#' @param filename output file
#' @param count Add total count of cases in the beginning
#' @return Table indicating the count for each unique entry in the input  vector or matrix. The function writes the statistics in the file.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{tab <- write_xtable(x, "tmp.tab")}
#' @keywords utilities
write_xtable <- function (x, filename, count = FALSE) {

  message(paste("Writing", filename))

  if (is.factor(x)) {
    x <- as.character(x)
  }
 
  if (is.vector(x)) {
  
    # Remove NAs
    x <- x[!is.na(x)]

    if (length(x) == 0) {
      write("The input list is empty.", file = filename)
      return(NULL)
    }

    x <- as.character(x)
    x[is.na(x)] <- "N/A"
    counts <- rev(sort(table(x)))
    tab <- data.frame(list(Name = names(counts), Count = as.vector(counts)))

  } else if (is.matrix(x) | is.data.frame(x)) {

    id <- apply(x, 1, function (x) {paste(x, collapse = "-")})
    ido <- rev(sort(table(id)))
    idn <- ido[match(id, names(ido))]
    
    tab <- cbind(x, Count = idn)
    tab <- tab[rev(order(as.numeric(tab[, "Count"]))),]
    tab <- tab[!duplicated(tab),]

    rownames(tab) <- NULL

  }

  if (count) {
    if (nrow(tab) > 1) {
      tab <- apply(tab, 2, as.character)
    }
    n <- sum(as.numeric(tab[,"Count"]), na.rm = TRUE)
    suppressWarnings(tab <- rbind(c("Total count: ", paste("n=", n, " (", round(100*n/length(x), 2), "% non-NA values)", sep = "")), tab))
  }

  write.table(tab, file = filename, quote = FALSE, sep = "\t", row.names = FALSE)

  tab

}
