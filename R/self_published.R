#' @title Identify Self Published Documents
#' @description Identify Self published documents.
#' @param df data.frame that includes the given field
#' @return Output of the polished field
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @details The function considers self published documents those that have 'author' (or similar) in the publisher field, and those where the publisher is unknown.
#' @examples \dontrun{a <- polish_field(df, "title")}
#' @export
#' @keywords utilities
self_published <- function (df) {

  publisher <- as.character(df$publisher)
  author <- as.character(df$author)
  author[author == "NA"] <- NA

  inds <- which(grepl("^<*author>*$", tolower(publisher)) & !is.na(author))
  if (length(inds)>0) {
    publisher[inds] <- author[inds]
  }
  inds2 <- which(grepl("^<*author>*$", tolower(publisher)) & is.na(author))
  if (length(inds2)>0) {
    publisher[inds2] <- NA # "Self-published (unknown author)"
  }
  selfpub <- as.logical((publisher %in% "Self-published (unknown author)") |
  	                (author == publisher)
			)

  inds3 <- which(publisher %in% c("tuntematon", "unknown", "anonymous", "NA"))
  publisher[inds3] <- NA

  # Enrich NA self-published cases from the known ones
  nainds <- which(is.na(selfpub))
  if (length(nainds) > 0 & length(selfpub) > 0) {
    nots <- nainds[df[nainds, "publisher"] %in% df[which(!selfpub), "publisher"]]
    selfpub[nots] <- FALSE
    yess <- nainds[df[nainds, "publisher"] %in% df[which(selfpub), "publisher"]]
    selfpub[yess] <- TRUE
  }
  
  # Mark remaining self-published NAs to FALSE by default
  # selfpub[is.na(selfpub)] <- FALSE

  data.frame(publisher = publisher,
             self_published = selfpub)

}