#' @title Update Preprocessed Data File
#' @description Update Preprocessed Data File.
#' @param file File name
#' @param df.preprocessed Preprocessed data.frame
#' @return File name
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{f <- update_preprocessed_data_file(file)}
#' @keywords utilities
update_preprocessed_data_file <- function (file, df.preprocessed) {

  spl <- strsplit(file, "/")
  file <- spl[[length(spl)]]
  dir <- paste(spl[-length(spl)], collapse = "/")

  if (file %in% dir(dir, full.names = TRUE)) {
    # Read the previously preprocessed file, and replace the updated
    # fields with the new versions
    df <- readRDS(df.preprocessed, file)
    df <- df[, setdiff(names(df), names(df.preprocessed))]
    # TODO add merging based on document identifiers to make this step
    # more reliable
    df <- cbind(df, df.preprocessed)
    saveRDS(df, file = file, compress = TRUE)
  } else {
    saveRDS(df.preprocessed, file = file, compress = TRUE)
  }
 
  file
  
}

