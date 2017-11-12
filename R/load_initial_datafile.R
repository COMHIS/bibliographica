#' @title Load Intial Datafile
#' @description Load datafile in csv.gx format, or use previously saved Rds.
#' @param datafile Datafile in csv.gz format
#' @param ignore.fields Fields to ignore
#' @param reload.data Boolean, edfault FALSE. Use datafile or previously saved Rds?
#' @return data.frame with raw data fields
#' @export
#' @author Leo Lahti, Ville Vaara
#' @references See citation("bibliographica")
#' @examples \dontrun{df.orig <- load_initial_datafile(datafile, ignore.fields, reload.data = TRUE)}
#' @keywords utilities
load_initial_datafile <- function(datafile, ignore.fields, reload.data = FALSE) { 

  if (reload.data) {

    message("Reading reloaded data. This may take a while ...")
    # Read the raw data
    df.orig <- read_bibliographic_metadata(datafile,
      ignore.fields = ignore.fields,
      verbose = TRUE)

    # Save the raw data
    message("Saving data as Rds...")
    saveRDS(df.orig, file = "df.raw.Rds", compress = "xz")
    message("Data saved!")

  } else {

    message("Reading data previously saved as Rds.")
    df.orig <- readRDS("df.raw.Rds")

  }

  return (df.orig)

}

