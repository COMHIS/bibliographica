#' @title Polish Dimensions
#' @description Polish dimension field for many documents at once.
#' @param x A vector of dimension notes
#' @param fill Logical. Estimate and fill in the missing information: TRUE/FALSE
#' @param dimtab Dimension mapping table
#' @param verbose verbose
#' @param synonyms Synonyme table
#' @param sheet.dimension.table Sheet dimension info
#' @return Dimension table
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # polish_dimensions(c("2fo", "14cm"), fill = TRUE)
#' @keywords utilities
polish_dimensions <- function (x, fill = TRUE, dimtab = NULL, verbose = FALSE, synonyms = NULL, sheet.dimension.table = NULL) {

  s <- as.character(x)

  if (is.null(dimtab)) {
    if (verbose) {
      message("dimtab dimension mapping table not provided, using the default table dimension_table()")
    }
    dimtab <- dimension_table()
  }
  if (is.null(sheet.dimension.table)) {
    sheet.dimension.table <- sheet_area(verbose = FALSE)
  }

  if (is.null(synonyms)) {
    f <- system.file("extdata/harmonize_dimensions.csv", package = "bibliographica")
    synonyms <- read_mapping(f, sep = "\t", mode = "table",encoding = "UTF-8")
  } 

  if (verbose) { message("Initial harmonization..") }
  s <- tolower(s)

  sorig <- s
  s <- suniq <- unique(s)

  # 75,9 -> 75.9  
  s <- gsub(",", ".", s)
  s <- gsub("lon.", "long ", s)   
  s <- gsub(" /", "/", s)

  # Harmonize the terms
  s <- map(s, synonyms, mode = "recursive")

  # Remove brackets
  s <- gsub("\\(", " ", gsub("\\)", " ", s)) 
  s <- gsub("\\[", " ", gsub("\\]", " ", s))
  # Add spaces
  s <- gsub("cm\\. {0,1}", " cm ", s)  
  s <- gsub("x", " x ", s)
  s <- gsub("oblong", "obl ", s)
  s <- gsub("obl\\.{0,1}", "obl ", s)  
  # Remove extra spaces
  s <- gsub(" /", "/", s)
  s <- condense_spaces(s)

  # "16mo in 8's."
  inds <- grep("[0-9]+.o in [0-9]+.o", s)  
  s[inds] <- gsub(" in [0-9]+.o", "", s[inds])

  # "12 mo
  inds <- grep("[0-9]+ .o", s)  
  for (id in c("mo", "to", "vo", "fo")) {
    s[inds] <- gsub(paste(" ", id, sep = ""), id, s[inds])
  }

  s <- harmonize_dimension(s, synonyms) 
  s <- map(s, synonyms, mode = "recursive")  

  # Make it unique here: after the initial harmonization
  # This helps to further reduce the number of unique cases 
  # Speed up by only handling unique cases
  # Temporarily map to original indices to keep it clear
  s <- s[match(sorig, suniq)]  
  sorig <- s
  s <- suniq <- unique(sorig)

  if (verbose) {
    message(paste("Estimating dimensions:", length(suniq), "unique cases"))    
  }

  # --------------------------------------

  tab <- t(sapply(s, function (x) {a <- try(polish_dimension(x, synonyms)); if (class(a) == "try-error") {a <- rep(NA, 5)}; return(a)
    }))
  rownames(tab) <- NULL

  tab <- data.frame(tab)

  if (verbose) {
    message("Convert to desired format")    
  }
  tab$original <- as.character(tab$original)
  tab$gatherings <- order_gatherings(unlist(tab$gatherings))
  tab$width <- suppressWarnings(as.numeric(as.character(tab$width)))
  tab$height <- suppressWarnings(as.numeric(as.character(tab$height)))
  tab$gatherings <- order_gatherings(tab$gatherings)
  tab$obl <- unlist(tab$obl, use.names = FALSE)
  tab.original <- tab  

  tab.final <- tab.original
  colnames(tab.final) <- paste0(colnames(tab.original), ".original")

  if (fill) {
    if (verbose) {
      message("Estimating missing entries")
    }

    tab.estimated <- augment_dimension_table(tab.original, dim.info = dimtab, sheet.dim.tab = sheet.dimension.table, verbose = verbose)

    tab.final <- cbind(tab.final, tab.estimated)

  }

  tab.final$original.original <- NULL  

  tab.final <- tab.final[match(sorig, suniq),]

  tab.final

}



