#' @title Write Summary Table
#' @description Write xtable in a file
#' @param x a vector or matrix
#' @param filename output file
#' @param count Add total count of cases in the beginning
#' @param sort.by Column used for sorting. The Count is the default.
#' @return Table indicating the count for each unique entry in the input  vector or matrix. The function writes the statistics in the file.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{tab <- write_xtable(x, "tmp.tab")}
#' @keywords utilities
write_xtable <- function (x, filename = NULL, count = FALSE, sort.by = "Count") {

  if (length(x) == 0) {  
    message("The input to write_table is empty.")
      write("The input list is empty.", file = filename)    
    return(NULL)
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  tab <- NULL

  if (is.vector(x)) {
  
    # Remove NAs
    x <- x[!is.na(x)]

    if (length(x) == 0 && !is.null(filename)) {
      write("The input list is empty.", file = filename)
      return(NULL)
    }

    x <- as.character(x)
    x[is.na(x)] <- "N/A"
    counts <- rev(sort(table(x)))
    tab <- data.frame(list(Name = names(counts), Count = as.vector(counts)))

    if (is.null(filename)) {return(tab)}

  } else if (is.matrix(x) || is.data.frame(x)) {

    if (is.null(colnames(x))) {
      colnames(x) <- paste("X", 1:ncol(x), sep = "")
    }

    id <- apply(x, 1, function (x) {paste(x, collapse = "-")})
    ido <- rev(sort(table(id)))
    idn <- ido[match(id, names(ido))]

    tab = x
    tab$Count = idn


    tab <- tab[!duplicated(tab),]

    if (is.null(filename)) {
      tab = tab[rev(order(tab$Count)),]
      rownames(tab) = NULL
      return(tab)
    }

    if (length(tab) > 0) {
      tab <- as.matrix(tab, nrow = nrow(x))
      if (ncol(tab) == 1) { tab <- t(tab) }
      colnames(tab) <- c(colnames(x), "Count")
      rownames(tab) <- NULL
    } else {
      tab <- NULL
    }
    
  }


  # Arrange
  if (!sort.by %in% c("Count", colnames(x))) {
    #warning("Sorting by name")
    sort.by <- "Name"
  }

  s <- as.character(tab[, sort.by])
  n <- as.numeric(s)
  if (all(!is.na(n[!is.na(s)]))) {
    # If all !NAs are numeric
    o <- rev(order(n))
  } else {
    # Consider as char
    o <- order(s)
  }
  tab <- tab[o,]

  
  if (count) {
    if (is.null(dim(tab)) && !is.null(tab)) {
      tab <- t(as.matrix(tab, nrow = 1))
    }
    if (!is.null(tab) && nrow(tab) > 1) {
      tab <- apply(tab, 2, as.character)
    }
    n <- sum(as.numeric(tab[, "Count"]), na.rm = TRUE)
    if (is.matrix(tab)) {
      suppressWarnings(tab <- rbind(rep("", ncol(tab)), tab))
      tab[1, 1] <- "Total count: "
      tab[1, 2] <- n
      if (ncol(tab)>2) {
        tab[1, 3:ncol(tab)] <- rep("", ncol(tab) - 2)
      }
    } else {
      tab <- c(paste("Total count:", n), tab)
    }
    
  }

  if (!is.null(filename)) {
    message(paste("Writing", filename))
    write.table(tab, file = filename, quote = FALSE, sep = "\t", row.names = FALSE)
  }
  
  tab

}
