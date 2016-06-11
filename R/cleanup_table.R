#' @title Clean synonyme table
#' @description Clean up the synonyme table.
#'
#' @param f File name
#' @param sep Separator
#' @param write Write back to file after cleaning up ? Logical.
#' @return Polished synonyme table.
#' @details Adds, lowercase version of each name; removes duplicates;
#'          writes back to file (optional)
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %<>%
#' @export
#' @examples # tmp <- cleanup_synonyme_table(f, sep = ";", write = FALSE)
#' @keywords utilities
cleanup_synonyme_table <- function (f, sep = ";", write = FALSE) {

  # Harmonize places with synonyme table
  # system("cp PublicationPlaceSynonymes.csv ~/tmp/"); s <- cleanup_synonyme_table("PublicationPlaceSynonymes.csv", sep = ";", write = TRUE)
  # f <- system.file("extdata/PublicationPlaceSynonymes.csv", package = "bibliographica")
		
  synonymes <- read.csv(f, sep = sep, stringsAsFactors = FALSE)

  # Ensure all synonymes are lowercase
  synonymes$synonyme <- tolower(synonymes$synonyme)
  
  # Add lowercase version of each name to the synonymes
  synonymes2 <- data.frame(list(name = synonymes$name, synonyme = tolower(synonymes$name), comment = synonymes$comment))
  synonymes <- bind_rows(synonymes, synonymes2)

  # Remove NA comments
  inds <- which(is.na(synonymes$comment) | synonymes$comment == "")
  synonymes$comment[inds] <- ""

  # Remove duplicates
  synonymes <- synonymes[!duplicated(synonymes),]

  # Sort by name
  name <- NULL # Avoid warnings in build
  synonymes %<>% arrange(name)   
  
  # Write to file
  if (write) {
    message(paste("Overwriting file:", f))
    write.table(synonymes, sep = sep, quote = FALSE, row.names = FALSE, file = f)
  }

  synonymes

}

