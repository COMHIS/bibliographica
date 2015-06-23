#' @title dimension_table
#' @description Document dimension mapping table
#'
#' @param ... Arguments to be passed
#' @return Document dimension table
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples dimension_table()
#' @keywords utilities
dimension_table <- function (...) {

  f <- system.file("extdata/documentdimensions.csv", package = "bibliographica")
  dd <- read.csv(f, header = TRUE)[-1,]
  colnames(dd) <- gsub("^X", "", colnames(dd))
  colnames(dd)[[1]] <- "height"
  colnames(dd) <- gsub("NA.", "NA", colnames(dd))

  dd <- apply(dd, 2, as.character)
  
  # Add 1to
  ss <- sheet_sizes()
  dd <- cbind(dd, "1to" = rep("x", nrow(dd)))
  row1 <- rep("x", ncol(dd))
  dd <- rbind(row1, dd)  
  dd[, "height"] <- as.character(dd[, "height"])
  dd[,"NA"] <- as.character(dd[,"NA"])
  dd[,"1to"] <- as.character(dd[,"1to"])
  dd[1, "height"] <- ss[ss$gatherings == "1to", "height"]
  dd[1, "NA"] <- ss[ss$gatherings == "1to", "width"]
  dd[1, "1to"] <- ss[ss$gatherings == "1to", "width"]

  # Reorder columns
  rownames(dd) <- NULL

  dd <- as.data.frame(dd, stringsAsFactors = FALSE)

  dd

}
