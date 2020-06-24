#' @title Load Intial Datafile
#' @description Load datafile in csv.gz format, or use previously saved Rds.
#' @param datafile Datafile in csv.gz format
#' @param reload.data Boolean, edfault FALSE. Use datafile or previously saved Rds?
#' @param output.file Output file for storing the resulting object.
#' @param sep separator
#' @return data.frame with raw data fields
#' @export
#' @author Leo Lahti, Ville Vaara
#' @references See citation("bibliographica")
#' @examples \dontrun{df.orig <- load_initial_datafile(datafile, reload.data = TRUE)}
#' @keywords utilities
load_initial_datafile <- function(datafile, reload.data = FALSE, output.file = "parsed.Rds", sep = "|") { 

  if (reload.data) {

    message("Reading reloaded data. This may take a while ...")
    # Read the raw data
    df.orig <- read_bibliographic_metadata(datafile, verbose = TRUE, sep = sep)

    # Save the raw data
    message("Saving data as Rds...")
    saveRDS(df.orig, file = output.file, compress = "xz")
    message("Data saved!")

  } else {

    message("Reading data previously saved as Rds.")
    df.orig <- readRDS(output.file)

  }

  return (df.orig)

}

