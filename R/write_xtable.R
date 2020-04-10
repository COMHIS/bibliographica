#' @title Write Summary Table
#' @description Write xtable in a file
#' @param x a vector or matrix
#' @param filename output file
#' @param count Add total count of cases in the beginning
#' @param sort.by Column used for sorting. The Count is the default.
#' @param na.rm Remove NA entries
#' @param add.percentages Add percentage information to the table. This indicates the total fraction of the count, calculated from all input entries if na.rm is FALSE, and from non-NA entries if na.rm is TRUE.
#' @return Table indicating the count for each unique entry in the input  vector or matrix. The function writes the statistics in the file.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{tab <- write_xtable(x, "tmp.tab")}
#' @keywords utilities
write_xtable <- function (x, filename = NULL, count = FALSE, sort.by = "Count", na.rm = TRUE, add.percentages = FALSE) {

  xorig <- x

  if (is.data.frame(x) && ncol(x) == 1) {
    x <- as.vector(x[,1])
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  # Remove NAs
  if (na.rm) {
    if (is.null(dim(x))) {
      x <- x[!is.na(x)]
    } else {
      keep <- which(rowMeans(is.na(x)) < 1)
      if (length(keep) > 0) {
        x <- x[keep,]
      } else {
        x <- NULL
      }
    }
  }

  if (length(x) == 0) {  
    message("The input to write_table is empty.")
    write("The input list is empty.", file = filename)    
    return(NULL)
  }

  tab <- NULL
  if (is.vector(x)) {

    # Original number of entries (before removing NAs)
    ntotal <- length(x)

    if (length(x) == 0 && !is.null(filename)) {
      write("The input list is empty.", file = filename)
      return(NULL)
    }

    if (count) {
      counts <- rev(sort(table(x)))
      tab <- data.frame(list(Name = names(counts), Count = as.vector(counts)))
    }
    
    if (is.null(filename)) {return(tab)}

  } else if (is.matrix(x) || is.data.frame(x)) {

    # Original number of entries (before removing NAs)
    ntotal <- nrow(x)
    
    if (is.null(colnames(x))) {
      colnames(x) <- paste("X", 1:ncol(x), sep = "")
    }
    
    # Proceed
    id <- apply(x, 1, function (x) { paste(x, collapse = "-") })
    ido <- rev(sort(table(id)))
    
    tab <- as.data.frame(x)

    if (count) { 
      idn <- ido[match(id, names(ido))]
      tab[, "Count"] <- idn
    }
    
    tab <- tab[!duplicated(tab),]

    if (is.null(filename) & count) {
      tab <- tab[rev(order(tab[, "Count"])),]
      rownames(tab) <- NULL
      return(tab)
    }

    if (length(tab) > 0) {
      tab <- as.matrix(tab, nrow = nrow(x))
      if (ncol(tab) == 1) { tab <- t(tab) }
      # HR: Fixing a bug: "Count" had been tried to add twice as a column name
      if (count & !"Count" %in% colnames(tab)) {
        colnames(tab) <- c(colnames(tab), "Count")
      }
      rownames(tab) <- NULL
    } else {
      tab <- NULL
      
      
    }    
  }

  # Arrange
  if (!sort.by %in% c("Count", colnames(x))) {
    sort.by <- "Name"
  }

  s <- as.character(tab[, sort.by])
  n <- suppressWarnings(as.numeric(s))
  if (all(!is.na(n[!is.na(s)]))) {
    # If all !NAs are numeric
    o <- rev(order(n))
  } else {
    # Consider as char
    o <- order(s)
  }
  tab <- tab[o,]

  # Add fraction
  if (add.percentages & count) {
    tab <- cbind(tab,
      Percentage = round(100 * as.numeric(condense_spaces(tab[, "Count"]))/ntotal, 2))
  }

  if (count) {

    if (is.null(dim(tab)) && !is.null(tab)) {
      tab <- t(as.matrix(tab, nrow = 1))
    }
    
    if (!is.null(tab) && nrow(tab) > 1) {
      tab <- apply(tab, 2, as.character)
    }

    n <- sum(as.numeric(tab[, "Count"]), na.rm = TRUE)

    ntxt <- n    
    if (is.matrix(tab)) {
      suppressWarnings(tab <- rbind(rep("", ncol(tab)), tab))
      tab[1, 1] <- paste("Total count (na.rm ", na.rm, "): ", sep = "")
      tab[1, 2] <- ntxt
      if (ncol(tab)>2) {
        tab[1, 3:ncol(tab)] <- rep("", ncol(tab) - 2)
      }
    } else {
      tab <- c(paste("Total count:", ntxt), tab)
    }    
  }

  if (!is.null(filename)) {
    message(paste("Writing", filename))
    write.table(tab, file = filename, quote = FALSE, sep = "\t", row.names = FALSE)
  }

  if (!count && ("Count" %in% names(tab))) {
    tab <- tab[, -ncol(tab)]
  }

  tab

}
