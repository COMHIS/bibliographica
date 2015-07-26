#' @title read_bibliographic_metadata
#' @description Read preparsed metadata
#'
#' @param file Source Parsed data file
#' @return data.frame with raw data fields
#'
#' @importFrom dplyr tbl_df
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{df.raw <- read_bibliographic_metadata(file)}
#' @keywords utilities

read_bibliographic_metadata <- function (file) {
  
  # Read data
  tab <- read.csv(file, sep = "|", strip.white = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")

  # Clean up a bit
  # TODO: add clarification what sort of cases this is meant to handle?
  tab <- apply(tab,1:2,function(x){
    x <- gsub("^[[:space:],:;]+","",gsub("[[:space:],:;]+$","",x)) 
    x
  })

  # Convert empty cells to NAs
  tab <- apply(tab, 2, function (x) {y <- x; y[x %in% c(" ", "")] <- NA; y})
  
  # Form data frame
  df <- tbl_df(as.data.frame(tab, stringsAsFactors = FALSE))

  # Pick field clear names
  names.orig <- names(df)
  names(df) <- harmonize_field_names(gsub("^X", "", names(df)))

  if (any(is.na(names(df)))) {
    warnings(paste("Fields", paste(names.orig[which(is.na(names(df)))], collapse = ";"), "not recognized"))
  }
  
  df

}


  # All ESTC fiels 100a|100d|240n|245a|260a|260b|260c|300a|300c|650a|650y,651y|650z,651a,651z
  # All Fennica fields 041a|041h|100a|100d|240a|245a|245b|260a|260b|260c|300a|300b|300c|300e|310a|362a|500a|502a|502c|502d|510a|510c|650a|651a|710a|720a|785t|852a  
  # All Kungliga fields 008lang|100a|100d|110a|240a|245a|245b|245c|260a|260b|260c|260e|260f|300a|300b|300c|300e|310a|362a|440a|440v|500a|502a|502c|502d|510a|510c|650a|650x|650y|650z|651a|700a|700d|710a|720a|740a|772c|772d|772t|785t|852a|852j|852z|866x|900a|900d|900u|976a|976b

