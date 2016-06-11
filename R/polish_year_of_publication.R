#' @title Polish Publication Year
#' @description Pick and polish year(s) of publication.
#' @param x publication time field (a vector)
#' @return data.frame for separate fields for books (year) and magazines (start, end)
#' @importFrom tau fixEncoding
#' @export
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_year_of_publication(c("1776","1803-1806"))}
#' @keywords utilities
polish_year_of_publication <- function(x) {
  year <- fixEncoding(x,latin1=TRUE)

  year <- gsub("\\-\\-\\-","\\-",year)
  year <- gsub("\\-\\-","\\-",year)
  year <- gsub("\\- ","\\-",year)
  year <- gsub(" \\-","\\-",year)
  year <- gsub("\\[","",year)
  year <- gsub("\\]","",year)
  year <- gsub("\\?","",year)
  year <- gsub("^s.a$",NA,year)
  year <- gsub("^s. a$",NA,year)
  year <- gsub("^S.a$",NA,year)
  year <- gsub("^S. a$",NA,year)
  year <- gsub("^[0-9]{1,3}$",NA,year)

  year_first <- year
  year_last <- year

  year <- gsub("^([0-9]{4})$","\\1",year)
  year <- gsub("^.* ([0-9]{4})$","\\1",year)
  year <- gsub("^([0-9]{4}) .*$","\\1",year)
  year <- gsub("^[0-9]{4}\\-[0-9]{4}",NA,year)
  year <- gsub("^.* [0-9]{4}\\-[0-9]{4}",NA,year)
  year <- gsub("^[0-9]{2}[0-9]{2}\\-[0-9]{2}",NA,year)
  year <- gsub("^.*\\;([0-9]{4})$","\\1",year)
  year <- gsub("^([0-9]{4})\\;.*$","\\1",year)
  year <- gsub("^([0-9]{4})\\,\\;.*$","\\1",year)
  year <- gsub("^n.([0-9]{4})$","\\1",year)  
  year <- gsub("^1892/1893\\-1900$",NA,year)
  year <- gsub("^(?![0-9]{4}$).+$",NA,year,perl=TRUE)
  year <- as.numeric(year)

  year_first <- gsub("^[0-9]{4}$",NA,year_first)
  year_first <- gsub("^.* [0-9]{4}$",NA,year_first)
  year_first <- gsub("^[0-9]{4} .*$",NA,year_first)
  year_first <- gsub("^([0-9]{4})\\-[0-9]{4}","\\1",year_first)
  year_first <- gsub("^.* ([0-9]{4})\\-[0-9]{4}","\\1",year_first)
  year_first <- gsub("^([0-9]{2})([0-9]{2})\\-[0-9]{2}","\\1\\2",year_first)
  year_first <- gsub("^.*\\;[0-9]{4}$",NA,year_first)
  year_first <- gsub("^[0-9]{4}\\;.*$",NA,year_first)
  year_first <- gsub("^[0-9]{4}\\,\\;.*$",NA,year_first)
  year_first <- gsub("^n.[0-9]{4}$",NA,year_first)
  year_first <- gsub("^1892/1893\\-1900$","1893",year_first)
  year_first <- as.numeric(year_first)

  year_last <- gsub("^[0-9]{4}$",NA,year_last)
  year_last <- gsub("^.* [0-9]{4}$",NA,year_last)
  year_last <- gsub("^[0-9]{4} .*$",NA,year_last)
  year_last <- gsub("^[0-9]{4}\\-([0-9]{4})","\\1",year_last)
  year_last <- gsub("^.* [0-9]{4}\\-([0-9]{4})","\\1",year_last)
  year_last <- gsub("^([0-9]{2})[0-9]{2}\\-([0-9]{2})","\\1\\2",year_last)
  year_last <- gsub("^.*\\;[0-9]{4}$",NA,year_last)
  year_last <- gsub("^[0-9]{4}\\;.*$",NA,year_last)
  year_last <- gsub("^[0-9]{4}\\,\\;.*$",NA,year_last)
  year_last <- gsub("^n.[0-9]{4}$",NA,year_last)
  year_last <- gsub("^1892/1893\\-1900$","1900",year_last)
  year_last <- as.numeric(year_last)

  data.frame(list(published_in = year, published_from = year_first, published_till = year_last))

}
