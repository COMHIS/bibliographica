#' @title Cheat Publishers
#' @description Get publisher list from an xml file
#' @return Data frame with possible publisher form and the preferred output
#' @export
#' @import XML
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
cheat_publishers <- function() {
  
  #r <- xmlTree
  # TODO: Load from website, if local version doesn't exist
  
  
  # 1a. Brackets: year: $in, $from, $till 
  # 1b. Brackets: kirjapaino (?)
  # 1c. Brackets: kaupunki
  # 1d. Brackets: remove osakeyhtiö etc..
  # 1e. Brackets: grep what's still left
  # 2.  Remove everything that's not needed
  
  
  # Determine the number of known labels
  f <- system.file("extdata/cn-skos.rdf", package = "bibliographica")
  tree <- xmlTreeParse(f, useInternalNodes = TRUE)
  node_count <- length(xpathApply(tree, path="//rdac:C10005//skos:altLabel | //rdac:C10005//skos:prefLabel", xmlValue))
  
  # Prepare variables
  rdf <- xmlParse("cn-skos.rdf")
  r <- xmlChildren(rdf)
  all_names <- cbind.data.frame(alt=character(node_count), pref=character(node_count), stringsAsFactors=FALSE)
  ind = 1
  
  # Get all the known labels of each company and the preferred orthography for each
  for (parentNode in xmlChildren(r$RDF)) {
    # There's also another node "skos:Concept" which has a child node "prefLabel", so we'll just skip it
    if (xmlName(parentNode) != "C10005") {next}
    
    kids <- xmlApply(parentNode, xmlValue)
  
    
    if (length(parentNode[which(names(kids)=="prefLabel")]) != 0) {
      # Get all the children with tags "altLabel" or "prefLabel" and put the only child "prefLabel" as their counterparts
      # simplified example: 
      # <rdac:C10005><skos:altLabel>Hege is stupid></skos:altLabel><skos:altLabel>Hege is an idiot></skos:altLabel><skos:prefLabel>Hege on idiootti</skos:prefLabel></rdac:C10005>
      # ->                  alt              pref
      # [1]      Hege is stupid  Hege on idiootti
      # [2]    Hege is an idiot  Hege on idiootti
      # [3]    Hege on idiootti  Hege on idiootti
      tmp_frame <- data.frame(cbind(alt=as.character(unlist(unname(kids[which((names(kids)=="altLabel") | (names(kids)=="prefLabel"))]))), pref=unlist(unname(kids["prefLabel"]))))
      
      # There's probably a less cumbersome way to do this, so feel free to change
      all_names[ind:(ind+(nrow(tmp_frame["alt"]))-1),1] <- lapply(tmp_frame["alt"], as.character)
      all_names[ind:(ind+(nrow(tmp_frame["alt"]))-1),2] <- lapply(tmp_frame["pref"], as.character)
      ind <- ind + nrow(tmp_frame["alt"])
    }
  }
  
  year=data.frame(year_from=integer(node_count), year_till=integer(node_count), stringsAsFactors=FALSE)
  year[any(year)==0] <- NA
  
  # Get the indices with years in brackets
  # Case: Stupid Hege Inc. (1975-2016)
  inds <- grep("\\([^)]*[0-9]{4}[-][0-9]{4}", all_names[,2])
  year$year_from[inds] <- as.integer(gsub(".*\\([^)]*([0-9]{4})[-][0-9]{4}.*", "\\1", all_names[,2][inds]))
  year$year_till[inds] <- as.integer(gsub(".*\\([^)]*[0-9]{4}[-]([0-9]{4}).*", "\\1", all_names[,2][inds]))
  
  # Case: Stupid Hege Inc. (1975)
  inds <- grep("\\([^)]*[0-9]{4}", all_names[,2])
  inds <- intersect(which(is.na(year$year_from)), inds)
  year$year_from[inds] <- year$year_till[inds] <- as.integer(gsub(".*\\([^)]*([0-9]{4}).*", "\\1", all_names[,2][inds]))
  
  # Get the town/city/village in brackets
  town <- character(node_count)
  
  #inds <- grep("\\([^)]*, ?[0-9]{4}", all_names[,1])
  inds <- grep("\\([^)]*([[:upper:]][[:lower:]]+), ?[0-9]{4}", all_names[,2])
  town[inds] <- gsub(".*\\([^)]*([[:upper:]][[:lower:]]+), ?[0-9]{4}.*", "\\1", all_names[,2][inds])
  
  alt <- clean_Finto_publisher(all_names$alt,language = "finnish")
  pref <- clean_Finto_publisher(all_names$pref, language = "finnish")
  df <- cbind.data.frame(alt=alt, pref=pref, town=town, year_from=year$year_from, year_till=year$year_till, stringsAsFactors=FALSE)
  # There's an altValue of "$b" which produces problems later on
  df <- df[df$alt != "$b",]
  df
}  
  
