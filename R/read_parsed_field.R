#' @title Load Parsed Field
#' @description Load parsed CSV fields.
#' @param f Datafile in csv
#' @param field Field to read
#' @param subfield Subfield to read
#' @param n n
#' @return data.frame with raw data fields
#' @export
#' @author Leo Lahti
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- read_parsed_fields(f)}
#' @keywords utilities
read_parsed_fields <- function(f, field = 300, subfield = "c", n = NULL) { 

  Field_code <- Subfield_code <- Record_seq <- Value <- NULL

  nam <- paste0(field, subfield)

  # Read the data
  x <- read.csv(f, sep = "\t")

  if (!is.null(n)) {
    x <- x[1:n, ]  
  }

  # ID field (035a: system control number)
  #id <- x %>% filter(Field_code == 35 & Subfield_code == "a") %>%
  #            select(Record_seq, Value)
  id <- x %>% select(Record_seq, estc_id)

  # Data field
  field <- x %>% filter(Field_code == field & Subfield_code == subfield) %>%
                 select(Record_seq, Value)

  # Join
  df <- full_join(id, field, by = "Record_seq")

  names(df) <- c("Record_seq", "estc_id", nam)  
  df[[nam]] <- as.character(df[[nam]])
  df$estc_id <- as.character(df$estc_id) # Was: system_control_number  

  df
  
}

