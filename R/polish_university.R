#' @title Polish University
#' @description Polish university names
#' @param x university field (a vector)
#' @param synonyms Synonyme table
#' @return polished university field (a vector)
#' @export
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{v <- polish_university(c("HY","Suomen yliopisto"))}
#' @keywords utilities
polish_university <- function(x, synonyms=NULL) {

  # Turun yliopisto ja Åbo Akademi perustettiin vasta aineiston loputtua  (1920 ja 1918, vastaavasti)
  
  x[x=="Suomen yliopisto"] <- "Helsinki" # tarkistettu julkaisupaikoista
  x[x=="HY"] <- "Helsinki"
  x[x=="Dorpat"] <- "Tartto"
  x[x=="Turun akatemia"] <- "Turku"
  x[x=="Turun akatemia,;Turun akatemia"] <- "Turku"
  x[x=="1700"] <- NA
  
  if (is.null(synonyms)) {
    f <- system.file("extdata/fi_end_years.csv", package = "bibliographica")
    synonyms <- read_mapping(f, sep = "\t", include.lowercase = TRUE, mode = "table")
  }
  x <- map(x, synonyms)

  # TODO conversion file
  # Check these are already in Fennica conversions?
  #x <- gsub("^.*Aleksan.*$","Helsinki",x)
  #x <- gsub("^.*Alexan.*$","Helsinki",x)
  #x <- gsub("^.*Helsink.*$","Helsinki",x)
  #x <- gsub("^.*Helsing.*$","Helsinki",x)
  #x <- gsub("^.*Tekn.*$","TKK",x)
  #x <- gsub("^.*Bern.*$","Bern",x)
  #x <- gsub("^.*Jena.*$","Jena",x)
  #x <- gsub("^.*Lund.*$","Lund",x)
  #x <- gsub("^.*Upps.*$","Uppsala",x)
  #x <- gsub("^.*Ups.*$","Uppsala",x)
  #x <- gsub("^.*Leipzig.*$","Leipzig",x)
  #x <- gsub("^.*Turk.*$","Turku",x)
  #x <- gsub("^.*Basel.*$","Basel",x)
  #x <- gsub("^.*Berlin.*$","Berliini",x)
  #x <- gsub("^.*Strängnäs.*$","Strängnäs",x)
  #x <- gsub("^.*Göteborg.*$","Göteborg",x)
  #x <- gsub("^.*Ludwigs.*$","Freiburg",x) # tarkista
  #x <- gsub("^.*Hannover.*$","Hannover",x) # "Technischen Hochschule Hannover", nyyään Hannoverin yliopisto
  #x <- gsub("^.*bingen.*$","Tübingen",x)
  #x <- gsub("^.*Königsberg.*$","Königsberg",x)
  #x <- gsub("^.*rich.*$","Zürich",x)
  #x <- gsub("^.*Montpellier.*$","Montpellier",x)
  #x <- gsub("^.*Paris.*$","Pariisi",x)
  #x <- gsub("^.*Wittenberg.*$","Wittenberg",x)

  x
}
