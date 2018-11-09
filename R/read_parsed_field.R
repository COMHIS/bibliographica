#' @title Load Parsed Field
#' @description Load parsed CSV fields.
#' @param f Datafile in csv
#' @param field Field to read
#' @param subfield Subfield to read
#' @return data.frame with raw data fields
#' @export
#' @author Leo Lahti
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- read_parsed_fields(f)}
#' @keywords utilities
read_parsed_fields <- function(f, field = 300, subfield = "c", n = NULL) { 

  # Read the data
  x <- read.table(f, sep = "\t", header = TRUE)

  if (!is.null(x)) {
    x <- x[1:n, ]  
  }

  # ID field (035a: system control number)
  id <- x %>% filter(Field_code == 35 & Subfield_code == "a") %>%
              select(Record_seq, Value)

  # Data field
  field <- x %>% filter(Field_code == field & Subfield_code == subfield) %>%
                 select(Record_seq, Value)

  # Join
  df <- full_join(id, field, by = "Record_seq")

  if (any(duplicated(df$Value.x))) {
    warning("Duplicated values in read_parsed_fields. Returning NULL.")
    return(NULL)
  }

  names(df) <- c("Record_seq", "system_control_number", "value")  

  df
  
}

