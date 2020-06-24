#' @title Read Bibliographic Metadata
#' @description Read metadata parsed from XML.
#' @param file Parsed raw data file/s
#' @param verbose verbose
#' @param sep separator
#' @return data.frame with raw data fields
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df.raw <- read_bibliographic_metadata(file)}
#' @keywords utilities
read_bibliographic_metadata <- function (file, verbose = FALSE, sep = "|") {

  if (length(file) == 0) {stop("File is empty - halting !")}

  # If there are multiple files, read in a list and combine
  # in the end
  if (length(file) > 1) {

    dfs <- list()
    for (f in file) {
      if (verbose) {message(f)}
      dfs[[f]] <- read_bibliographic_metadata(f)
    }
    if (verbose) {message("Combining the batches..")}    
    df.all <- bind_rows(dfs)
    if (verbose) {message("OK")}    

    # Replace individual identifier columns
    df.all$original_row <- 1:nrow(df.all)

    return(df.all)

  } else {
  
    # Read data
    tab <- read.csv(file, sep = sep, strip.white = TRUE,
    	   		  stringsAsFactors = FALSE, encoding = "UTF-8")

    # Removes additional whitespace and some special characters from
    # beginning and end of strings
    tab <- apply(tab,1:2,function(x){
      x <- gsub("^[[:space:],:;]+","",gsub("[[:space:],:;]+$","",x)) 
    })

    # Convert empty cells to NAs
    tab <- apply(tab, 2, function (x) {y <- x; y[x %in% c(" ", "")] <- NA; y})
  
    # Form data frame
    df <- as.data.frame(tab, stringsAsFactors = FALSE)

    # Pick field clear names
    names.orig <- names(df)
    names(df) <- harmonize_field_names(gsub("^X", "", names(df)))

    if (any(is.na(names(df)))) {
      warnings(paste("Fields", paste(names.orig[which(is.na(names(df)))], collapse = ";"), "not recognized"))
    }

    df <- tbl_df(df)

    # Add identifier column
    df$original_row <- 1:nrow(df)

    return(df)

  }

}

  # CERL ?
  # All ESTC fiels 100a|100d|240n|245a|260a|260b|260c|300a|300c|310a|362a|650a|650y,651y|650z,651a,651z
  # All Fennica fields 041a|041h|100a|100d|240a|245a|245b|260a|260b|260c|300a|300b|300c|300e|310a|362a|500a|502a|502c|502d|510a|510c|650a|651a|710a|720a|785t|852a  
  # All Kungliga fields 008lang|100a|100d|110a|240a|245a|245b|245c|260a|260b|260c|260e|260f|300a|300b|300c|300e|310a|362a|440a|440v|500a|502a|502c|502d|510a|510c|650a|650x|650y|650z|651a|700a|700d|710a|720a|740a|772c|772d|772t|785t|852a|852j|852z|866x|900a|900d|900u|976a|976b

