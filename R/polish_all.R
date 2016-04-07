#' @title Polish all fields
#' @description Polish all fields.
#' @param df.orig Original data.frame 
#' @param fields Fields to be preprocessed.
#' @param verbose verbose
#' @param file Temporary results saved into a file 
#' @param mc.cores Number of cores for parallelization
#' @param conversions Field conversion list
#' @return Preprocessed data.frame and field conversion list.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{a <- polish_all(df.orig)}
#' @export
#' @keywords utilities
polish_all <- function (df.orig, fields = NULL, verbose = TRUE, file = NULL, mc.cores = 1, conversions = list()) {

  if (is.null(fields)) {
    message("List raw data fields to be preprocessed")
    fields <- names(df.orig) # Update all
    message("Entry identifier to match back to the originals")
    df.preprocessed <- data.frame(original_row = df.orig$original_row)
  } else {
    df.preprocessed <- df.orig
  }
  
  # List how raw data fields will be converted into
  # preprocessed data fields
  preprocessing.times <- c()

  # Preprocess the field only if it has to be updated
  for (field in fields) {

    message(field)

    # Starting time
    start.time <- Sys.time()

    # Polish the given field
    df.tmp <- polish_field(df.orig, field, verbose = FALSE, mc.cores = mc.cores)

    # List the output fields for this input field
    conversions[[field]] <- names(df.tmp)

    # Remove fields to be updated
    inds <- which(names(df.preprocessed) %in% unlist(conversions[[field]]))
    if (length(inds) > 0) { df.preprocessed <- df.preprocessed[, -inds]}

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

  list(df.preprocessed = df.preprocessed, conversions = conversions, preprocessing.times = preprocessing.times)

}





