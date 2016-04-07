#' @title Mark languages
#' @description Construct binary matrix of languages for each entry
#' @param x language field (a vector)
#' @return data.frame with separate fields for different languages
#' @export
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- mark_languages(c("fin;lat","eng"))}
#' @keywords utilities
mark_languages <- function(x) {

  x[x == "NA"] = ""
  x <- gsub("^;","",x)
  x <- gsub(";$","",x)

	subroutine <- function(abbrv){grepl(abbrv, x, ignore.case = T)}
	
	fin <- subroutine("fin")
	swe <- subroutine("swe")
	lat <- subroutine("lat")
	ger <- subroutine("ger")
	eng <- subroutine("eng")
	fre <- subroutine("fre")
	rus <- subroutine("rus")
	grc <- subroutine("grc")
	dan <- subroutine("dan")
	ita <- subroutine("ita")
	heb <- subroutine("heb")
	dut <- subroutine("dut")
	spa <- subroutine("spa")
	smi <- subroutine("smi")
	gre <- subroutine("gre")
	ice <- subroutine("ice")
	ara <- subroutine("ara")
	por <- subroutine("por")
	fiu <- subroutine("fiu")
	und <- subroutine("und")	
	mul <- subroutine("mul") | (sapply(strsplit(x, ";"), function (x) {length(unique(x))}) > 1)
	
	df = data.frame(list(finnish = fin, swedish = swe, latin = lat, german = ger, english = eng, french = fre, russian = rus, greek = grc, danish = dan, italian = ita, hebrew = heb,
	dutch = dut, spanish = spa, sami = smi, modern_greek = gre, icelandic = ice, arabic = ara, portuguese = por, finnougrian = fiu, multiple = mul, undetermined = und))

  df
  
}
