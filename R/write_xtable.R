#' @title write_xtable
#' @description Write xtable in a file
#' @param x a vector or matrix
#' @param filename output file 
#' @return Table indicating the count for each unique entry in the input  vector or matrix. The function writes the statistics in the file.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{tab <- write_xtable(x, "tmp.tab")}
#' @keywords utilities
write_xtable <- function (x, filename) {

  if (is.vector(x)) {	     

    counts <- rev(sort(table(x)))
    tab <- data.frame(list(Name = names(counts), Count = counts))

  } else if (is.matrix(x) | is.data.frame(x)) {

    id <- apply(x, 1, function (x) {paste(x, collapse = "-")})
    ido <- rev(sort(table(id)))
    idn <- ido[match(id, names(ido))]
    tab <- cbind(x, count = idn)
    tab <- tab[rev(order(as.numeric(tab[, "count"]))),]
    tab <- tab[!duplicated(tab),]
    colnames(tab) <- c(colnames(x), "count")
    rownames(tab) <- NULL

  }

  message(paste("Writing", filename))
  write.table(tab, file = filename, quote = FALSE, sep = "\t", row.names = FALSE)

  tab

}
