#' @title Update Preprocessed Data File
#' @description Update Preprocessed Data File.
#' @param file File name
#' @param df.preprocessed Preprocessed data.frame
#' @param mode "update" or "overwrite"; the former updates the given fields;
#'        the latter overwrites the file completely
#' @return File name
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{f <- update_preprocessed_data_file(file)}
#' @keywords utilities
update_preprocessed_data_file <- function (file, df.preprocessed, mode = "update") {

  spl <- unlist(strsplit(file, "/"))
  file <- spl[[length(spl)]]
  dir <- paste(spl[-length(spl)], collapse = "/")

  if (file %in% dir(dir, full.names = TRUE) & mode == "update") {
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

