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

  #print("Self-published docs where author is known but publisher not")
  inds <- which(tolower(publisher) %in% c("author", "<<author>>") & !is.na(author))
  if (length(inds)>0) {
    publisher[inds] <- author[inds]
  }

  #print("When author is unknown mark it as self published")
  inds2 <- which(tolower(publisher) == "author" & is.na(author))  
  inds3 <- which(publisher %in% c("tuntematon", "unknown", "anonymous"))
  inds <- union(inds2, inds3)
  if (length(inds)>0) {
    publisher[inds] <- "Self-published (unknown author)"
  }
  
  selfpub <- as.logical((publisher %in% "Self-published (unknown author)") | (author == publisher))

  data.frame(publisher = publisher, self_published = selfpub)

}