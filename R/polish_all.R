#' @title Generic Preprocessing for Bibliographic Metadata
#' @description Polish all fields.
#' @param df.orig Original data.frame 
#' @param fields Fields to be preprocessed.
#' @param verbose verbose
#' @param file Temporary results saved into a file 
#' @param conversions Field conversion list
#' @param languages Languages to be used in polishing
#' @return Preprocessed data.frame and field conversion list.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{a <- polish_all(df.orig)}
#' @export
#' @keywords utilities
polish_all <- function (df.orig, fields = NULL, verbose = TRUE, file = NULL, conversions = list(), languages = NULL) {

  if (is.null(fields)) {
    message("List raw data fields to be preprocessed")
    fields <- names(df.orig) # Update all
  }
  
  # Prioritize polishing of certain fields
  # (may be needed for the later ones)
  for (priority.field in c("publication_place", "publication_year")) {
    if (priority.field %in% fields) {
      fields <- c(priority.field, setdiff(fields, priority.field))
    }
  }
  
  # Anti-prioritize polishing of certain fields
  # (may require information from the other fields)
  for (last.field in c("publisher")) {
    if (last.field %in% fields) {
      fields <- c(setdiff(fields, last.field), last.field)
    }
  }

  message("Entry identifier to match back to the originals")
  df.preprocessed <- data.frame(original_row = df.orig$original_row)
    
  # List how raw data fields will be converted into
  # preprocessed data fields
  preprocessing.times <- c()

  message("Processing fields..")
  # Preprocess the field only if it has to be updated
  for (field in fields) {

    message(field)

    # Starting time
    start.time <- Sys.time()

    # Polish the given field
    df.tmp <- polish_field(df.orig, df.preprocessed, field, verbose = FALSE,
                           languages = languages)

    # Remove the fields to be updated
    inds <- which(names(df.preprocessed) %in% names(df.tmp))
    if (length(inds) > 0) { df.preprocessed <- df.preprocessed[, -inds]}

    # List the output fields for this input field
    conversions[[field]] <- names(df.tmp)

    # Add the newly preprocessed field
    df.preprocessed <- cbind(df.preprocessed, df.tmp)

    # Remove the temporary data.frame
    rm(df.tmp)

    # Monitor time
    stop.time <- Sys.time()
    preprocessing.times[[field]] <- difftime(stop.time, start.time, units = "mins")
    
    if (!is.null(file)) {
      check <- field    
      save(df.preprocessed, conversions, check, file = file, compress = TRUE)
      save(preprocessing.times, file = "preprocessing.times.RData")         
    }

    # Cleanup
    gc()
    
  }

  message("Field preprocessing ok.")
  list(df.preprocessed = df.preprocessed,
       conversions = conversions,
       preprocessing.times = preprocessing.times)

}





