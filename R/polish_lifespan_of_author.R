#' @title polish_lifespan_of_author
#' @description Pick and polish years of birth and death
#'
#' @param df Main dataframe
#' @return Main dataframe
#'
#' @export
#' 
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{df <- polish_lifespan_of_author(df)}
#' @keywords utilities
polish_lifespan_of_author <- function(df) {
  birth <- df$author_date
  birth <- gsub("^([0-9]{3,4})\\D[0-9]{3,4}$","\\1",birth)
  birth <- gsub("^fl. ([0-9]{3,4})\\D[0-9]{3,4}$",NA,birth)
  birth <- gsub("^n. ([0-9]{4})\\D[0-9]{4}$","\\1",birth)
  birth <- gsub("^([0-9]{4})\\Dn. [0-9]{4}$","\\1",birth)
  birth <- gsub("^n. ([0-9]{4})\\Dn. [0-9]{4}$","\\1",birth)
  birth <- gsub("^s. ([0-9]{4})$","\\1",birth)
  birth <- gsub("^s. n. ([0-9]{4})$","\\1",birth)
  birth <- gsub("^k. ([0-9]{4})$",NA,birth)
  birth <- gsub("^d. ([0-9]{4})$",NA,birth)
  birth <- gsub("^k. n. ([0-9]{4})$",NA,birth)
  birth <- gsub("^k. ennen ([0-9]{4})$",NA,birth)
  birth <- gsub("^k. viimeistään ([0-9]{4})$",NA,birth)
  birth <- gsub("^k. ([0-9]{4}) jälkeen$",NA,birth)
  birth <- gsub("^s. n. ([0-9]{4}), k. [0-9]{4}$","\\1",birth)
  birth <- gsub("^s. ([0-9]{4}), k. n. [0-9]{4}$","\\1",birth)
  birth <- gsub("^[0-9]{4}\\Dluku$",NA,birth)
  birth <- gsub("^eli vielä ([0-9]{4})$",NA,birth)
  birth <- gsub("^[0-9]$",NA,birth)
  birth <- gsub("^([0-9]{2,3})\\D[0-9]{2,3} e.Kr$","\\-\\1",birth)
  birth <- gsub("^n. ([0-9]{2,3})\\D[0-9]{2,3} e.Kr$","\\-\\1",birth)
  birth <- gsub("^([0-9]{2,3})\\D[0-9]{2,3} e. Kr$","\\-\\1",birth)
  birth <- gsub("^n. ([0-9]{2,3})\\D[0-9]{2,3} e. Kr$","\\-\\1",birth)
  birth <- gsub("^s. ehkä 1620-luvulla, k. 1694$",NA,birth)
  birth <- gsub("^s. 1630-luvulla, k. 1684$",NA,birth)
  birth <- gsub("^s. 1590-luvulla, k. 1651$",NA,birth)
  birth <- gsub("^k. 1616/1617$",NA,birth)
  birth <- gsub("^n. 20 e.Kr.-40 j.Kr$","-20",birth)
  birth <- gsub("^1600/1700\\-luku$",NA,birth)
  birth <- gsub("^eli 300\\-luvun puolivälissä$",NA,birth)
  birth <- gsub("^300-l. j. Kr$",NA,birth)
  birth <- gsub("^k. 1730-luvulla$",NA,birth)
  birth <- gsub("^k. vähän ennen vuotta 1600$",NA,birth)
  birth <- gsub("^n. 363-425 j.Kr$",NA,birth)
  birth <- gsub("^s. 1678, k. 1695 jälkeen$","1678",birth)
  birth <- gsub("^s. n. 1560, k. ennen 1617$","1560",birth)
  birth <- gsub("^s. viim. 1638, k. 1681$",NA,birth)
  birth <- gsub("^toiminta\\-aika 1770\\-luku$",NA,birth)
  df$birth_year <- as.numeric(birth)

  death <- df$author_date
  death <- gsub("^[0-9]{3,4}\\D([0-9]{3,4})$","\\1",death)
  death <- gsub("^fl. [0-9]{3,4}\\D([0-9]{3,4})$",NA,death)
  death <- gsub("^n. [0-9]{4}\\D([0-9]{4})$","\\1",death)
  death <- gsub("^[0-9]{4}\\Dn. ([0-9]{4})$","\\1",death)
  death <- gsub("^n. [0-9]{4}\\Dn. ([0-9]{4})$","\\1",death)
  death <- gsub("^s. ([0-9]{4})$",NA,death)
  death <- gsub("^s. n. ([0-9]{4})$",NA,death)
  death <- gsub("^k. ([0-9]{4})$","\\1",death)
  death <- gsub("^d. ([0-9]{4})$","\\1",death)
  death <- gsub("^k. n. ([0-9]{4})$","\\1",death)
  death <- gsub("^k. ennen ([0-9]{4})$",NA,death)
  death <- gsub("^k. viimeistään ([0-9]{4})$",NA,death)
  death <- gsub("^k. ([0-9]{4}) jälkeen$",NA,death)
  death <- gsub("^s. n. [0-9]{4}, k. ([0-9]{4})$","\\1",death)
  death <- gsub("^s. [0-9]{4}, k. n. ([0-9]{4})$","\\1",death)
  death <- gsub("^[0-9]{4}\\Dluku$",NA,death)
  death <- gsub("^eli vielä ([0-9]{4})$",NA,death)
  death <- gsub("^[0-9]$",NA,death)
  death <- gsub("^[0-9]{2,3}\\D([0-9]{2,3}) e.Kr$","\\-\\1",death)
  death <- gsub("^n. [0-9]{2,3}\\D([0-9]{2,3}) e.Kr$","\\-\\1",death)
  death <- gsub("^[0-9]{2,3}\\D([0-9]{2,3}) e. Kr$","\\-\\1",death)
  death <- gsub("^n. [0-9]{2,3}\\D([0-9]{2,3}) e. Kr$","\\-\\1",death)
  death <- gsub("^s. ehkä 1620-luvulla, k. 1694$","1694",death)
  death <- gsub("^s. 1630-luvulla, k. 1684$","1684",death)
  death <- gsub("^s. 1590-luvulla, k. 1651$","1651",death)
  death <- gsub("^k. 1616/1617$","1616",death)
  death <- gsub("^n. 20 e.Kr.-40 j.Kr$","40",death)
  death <- gsub("^1600/1700\\-luku$",NA,death)
  death <- gsub("^eli 300\\-luvun puolivälissä$",NA,death)
  death <- gsub("^300-l. j. Kr$",NA,death)
  death <- gsub("^k. 1730-luvulla$",NA,death)
  death <- gsub("^k. vähän ennen vuotta 1600$",NA,death)
  death <- gsub("^n. 363-425 j.Kr$",NA,death)
  death <- gsub("^s. 1678, k. 1695 jälkeen$",NA,death)
  death <- gsub("^s. n. 1560, k. ennen 1617$",NA,death)
  death <- gsub("^s. viim. 1638, k. 1681$","1681",death)
  death <- gsub("^toiminta\\-aika 1770\\-luku$",NA,death)
  df$death_year <- as.numeric(death)

  df
}
