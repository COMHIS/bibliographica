#' @title polish_dimensions
#' @description Polish dimension field for many documents at once
#' @param x A vector of dimension notes
#' @param fill Logical. Estimate and fill in the missing information: TRUE/FALSE
#' @param dimtab Dimension mapping table
#' @param verbose verbose
#' @param synonyms Synonyme table
#' @return Dimension table
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # polish_dimensions(c("2fo", "14cm"), fill = TRUE)
#' @keywords utilities
polish_dimensions <- function (x, fill = FALSE, dimtab = NULL, verbose = FALSE, synonyms = NULL) {

  s <- as.character(x)

  if (is.null(synonyms)) {
    f <- system.file("extdata/harmonize_dimensions.csv", package = "bibliographica")
    synonyms <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
  } 

  # Speed up by only handling unique cases
  suniq <- unique(s)
  match.inds <- match(s, suniq)

  if (verbose) {
    message(paste("Estimating dimensions:", length(suniq), "unique cases"))    
  }

  tab <- t(sapply(suniq, function (x) {
    polish_dimension(x, synonyms)
    }))
  rownames(tab) <- NULL
  tab <- data.frame(tab)
  tab <- tab[match.inds,]

  if (verbose) {
    message("Convert to desired format")    
  }
  tab$original <- as.character(tab$original)
  tab$gatherings <- order_gatherings(tab$gatherings)
  tab$width <- suppressWarnings(as.numeric(as.character(tab$width)))
  tab$height <- suppressWarnings(as.numeric(as.character(tab$height)))
  tab$gatherings <- order_gatherings(tab$gatherings)
  tab$obl <- unlist(tab$obl)
  tab.original <- tab 

  tab.final <- tab.original
  colnames(tab.final) <- paste0(colnames(tab.original), ".original")
  
  if (fill) {
    if (verbose) {
      message("Estimating missing entries")
    }

    tab.estimated <- augment_dimension_table(tab.original, dimtab = dimtab, verbose = verbose)

    tab.final <- cbind(tab.final, tab.estimated)
  }

  tab.final$obl.original <- NULL
  tab.final$original.original <- NULL  

  # Remove the 'original' fields      
  tab.final <- tab.final[, -grep("original", names(tab.final))] 

  tab.final

}



