
harmonize_pages <- function (x) {

  # Remove some special cases manually
  s <- harmonize_pages_specialcases(x)

  # Remove dimension info
  s <- remove_dimension(s)

  # Harmonize i.e.
  s <- harmonize_ie(s)

  # Romans
  s <- harmonize_romans(s) 

  # Cases such as "p. 66" -> 1 page 
  if ((length(grep("^p\\. [0-9]$", s)>0) || length(grep("^p\\. [0-9][0-9]$", s))) || length(grep("^p\\. [0-9][0-9][0-9]$", s))>0) {
    s <- 1
  }

  # TODO formulate rule for this
  s <- gsub("233-248 leaves", "233-248,", s)
  s <- gsub("205-216 leaves", "205-216,", s)
  s <- gsub("107-133 leaves", "107-133,", s)

  # p. without space
  s <- gsub("xlix-liip. ;", "xlix-lii", s)

  # Ff.
  s <- gsub("Ff\\. ", "Ff.,", s)

  # Rare cases
  s <- gsub("3\\.", "3,", s)
  s <- gsub("c1 \\.", "", s)
  s <- gsub("32t p\\.", "32 p.", s)
  s <- gsub("\\[1⁺\\]", "[1]", s)
  s <- gsub("1\\/ \\.$", ",1", s)
  s <- gsub("c1⁰\\.$", "", s)
  s <- gsub("\\[x\\]", " ", s)
  s <- gsub("\\+\\]", "]", s)
  s <- gsub("\\[2\\] single and \\[8\\] double leaves of plates", "[2],[8]", s)
  s <- gsub("\\[fewer than 50 pages\\]", " ", s)
  s <- gsub("\\[No pagination provided\\]", " ", s)
  s <- gsub("in various pagings", " ", s)
  s <- gsub(" and ", " ", s)
  s <- gsub("leaf", " leaf", s)
  s <- gsub("53a-62k", "53-62", s)
  s <- gsub("\\:bill", " ", s)
  s <- gsub("\\:ill", " ", s)
  s <- gsub("\\bill", " ", s)
  s <- gsub("bill\\.", " ", s)
  s <- gsub("\\?\\]", "\\]", s) # [8?]
  s <- gsub("\\+", " ", s)    

  s <- gsub("[0-9][0-9] no\\. ;", " ", s)
  s <- gsub("[0-9] no\\. ;", " ", s)
  s <- gsub("[0-9][0-9][0-9] columns", " ", s)

  s <- gsub("bis", "", s)
  s <- gsub("\\*", "", s)
  s <- gsub("\\] p. \\[", "] p., [", s)
  s <- gsub("\\[\\?\\]", " ", s)
  s <- gsub("\\?", " ", s)
  s <- gsub("+\\}", "]", s)
  #s <- gsub("[0-9] pts in 1 v\\.", " ", s)
  s <- gsub("ca\\.", " ", s)

  # Add spaces around parentheses
  s <- gsub("\\]", "] ", s)
  s <- gsub("\\[", " [", s)
  s <- gsub("\\)", ") ", s)
  s <- gsub("\\(", " (", s)
  s <- condense_spaces(s)

  # Remove endings
  for (i in 1:5) {
    s <- str_trim(remove_endings(s, c(" ", "\\.", "\\,", "\\;", "\\:")))
  }

  # Harmonize sheet, plate and table info
  s <- harmonize_sheets(s)

  # Pp. -> p etc.
  s <- harmonize_page_info(s)
  
  # 1/4to etc -> 4
  #s <- gsub("1/", " ", s)

  # Remove spaces around dashes
  s <- gsub(" -", "-", s)
  s <- gsub("- ", "-", s)

  # Remove spaces around parentheses
  s <- str_trim(gsub("\\)", " ", gsub("\\(", " ", s)))
  #s <- gsub(" \\[", ",[", s) # 438[i.e 428] must come without comma
  s <- gsub(" \\(", ",(", s)
  s <- gsub("\\,\\,", ",", s)
  s <- gsub("^\\(", "", s)
  s <- gsub("\\)$", "", s)
  s <- condense_spaces(s)

  # Add commas
  #"[2] 4 p." -> "[2], 4 p."
  inds <- setdiff(1:length(s), grep("\\[i", s))
  s[inds] <- gsub(" \\[", "\\,[", s[inds])
  for (n in 0:9) {
    s <- gsub(paste("] ", n, sep = ""), paste("], ", n, sep = ""), s)
  }
  s <- gsub("leaves \\[", "leaves, [", s)
  s <- gsub("leaf \\[", "leaf, [", s)
  s <- gsub("\\] \\[", "], [", s)
  s <- gsub("p \\[", "p, [", s)
  s <- gsub("p \\(", "p, (", s)
  s <- gsub("p\\. \\[", "p, [", s)
  s <- gsub("p\\. \\(", "p, (", s)
  s <- gsub("p\\.\\]", "p]", s)

  if (length(grep("^p[0-9]", s))) {
    s <- substr(s, 2, nchar(s))
  }

  s <- str_trim(gsub("^,", "", s))
  if (is.na(s) || s == "") { s <- NA }

  s

}



harmonize_page_info <- function (s) {

  # Harmonize page info
  s <- gsub("^Pp\\.", " ", s)
  s <- gsub("^p\\. ", " ", s)
  s <- gsub("Pp\\.", "p", s)
  s <- gsub("pp\\.", "p", s)
  s <- gsub("p\\.", "p", s)
  s <- gsub("p ", "p", s)
  s <- gsub("p$", "p", s)
  s <- gsub("P\\.", "p", s)
  s <- gsub("P ", "p", s)
  s <- gsub("P$", "", s)
  s <- gsub("p$", "", s)  
  s <- gsub("P\\.$", "", s)
  s <- gsub("p\\.$", "", s)
  s <- gsub("p\\[", "p, [", s)

  s

}

harmonize_sheets <- function (s) {

  # Capitalization		 
  s <- gsub("Sheet", "sheet", s)

  # Plates
  s <- gsub("plates\\(one fold\\.\\)", "plates ", s)
  s <- gsub("p\\.plates", "p., plates", s) # "39,[1]p.plates :" -> "39,[1]p.,plates :"
  s <- gsub("\\)plates :", "),plates", s)
  s <- gsub("plates :", "plates ", s)
  s <- gsub("plate :", "plate ", s)
  s <- gsub("plates \\(some fold\\.\\) :", "plates ", s)
  s <- gsub("plates \\(one fold\\.\\)", "plates ", s)
  s <- gsub("p\\.table", "p., table", s)
  s <- gsub("p\\., of plates", "plates", s)

  s <- gsub("tables :", "tables ", s)
  s <- gsub("table :", "table ", s)

  # l.
  s <- gsub(" 1 l\\.", "leaf ", s) # 
  s <- gsub("\\[1\\] l\\.", "leaf ", s) # 
  s <- gsub("\\,l\\.", ",leaves ", s) # 
  s <- gsub(" l\\.", "leaves ", s) # 

  s <- gsub("  ", " ", gsub("folded", " folded", s))
  s <- gsub("\\(the last [0-9] p. blank\\)", "", s)
  s <- gsub("\\(versos blank\\)", " ", s)
  s <- gsub("\\(woodcut\\)", " ", s)
  s <- gsub("platess", "plates", s)
  s <- gsub("leaf of plates folded", "leaf", s)
  s <- gsub("leaves of plates folded", "leaf", s)
  s <- gsub("leaves of plates \\(maps\\)", "leaf", s)
  s <- gsub("leaves folded", "leaf", s)
  s <- gsub("fold\\. leaf of plates", "leaf", s)
  s <- gsub("folded leaf plates", "leaf", s)
  s <- gsub("folding leaf of plate", "leaf", s)
  s <- gsub("leaf plates", "leaf", s)
  s <- gsub("folded leaf", "leaf", s)
  s <- gsub("folded leaves", "leaves", s)
  s <- gsub("folded plates", "plates", s)
  s <- gsub("\\([0-9][0-9] folded\\)", " ", s)
  s <- gsub("\\([0-9] folded\\)", " ", s)
  s <- gsub("\\(some fold., col.\\)", " ", s)
  s <- gsub("\\(some fold\\., some col\\.\\)", " ", s)
  s <- gsub("\\(some col.\\)", " ", s)
  s <- gsub("\\(front\\.\\)", " ", s)
  s <- gsub("\\(some fold.\\)", " ", s)
  s <- gsub("\\([0-9] fold.\\)", " ", s)
  s <- gsub("\\([0-9] fold\\)", " ", s)
  s <- gsub("\\(some folded\\)", " ", s)
  s <- gsub("\\(most folded\\)", " ", s)
  s <- gsub("\\(one folded\\)", " ", s)
  s <- gsub("\\(folded\\)", " ", s)
  s <- gsub("\\(folding\\)", " ", s)
  s <- gsub("\\(some fold\\)", " ", s)
  s <- gsub("\\(fol\\.\\)", " ", s)
  s <- gsub("\\(\\[[0-9]\\] folded\\)", " ", s)
  s <- gsub("\\([0-9] folded\\)", " ", s)
  s <- gsub("fold\\.plates", "plates", s)
  s <- gsub("fold\\. plates", "plates", s)
  s <- gsub("fold\\.plate", "plate", s)
  s <- gsub("fold\\. plate", "plate", s)
  s <- gsub("folding plates", "plates", s)
  s <- gsub("foldplate", "plate", s)
  s <- gsub("folding", "plates", s)
  s <- gsub("folded plate", "plate", s)

  s <- gsub("leaves \\([0-9] folded\\)", "leaves ", s)
  s <- gsub("pate", "plate", s)
  s <- gsub("pleave", "leave", s)
  s <- gsub("plates plates", "plates", s)
  s <- gsub("leaf of plates", "leaf", s)
  s <- gsub("leaf of plate", "leaf", s)
  s <- gsub("leaves of plates", "leaves", s)
  s <- gsub("leaves of plate", "leaves", s)
  s <- gsub("leafs", "leaves", s)
  s <- gsub("[0-9]leaf", paste0(substr(s, 1, 1), " leaf"), s)
  s <- gsub("1 leaf", "1 sheet", s)
  s <- gsub("folded sheet", "sheet", s)
  s <- gsub("1sheet", "1 sheet", s)

  # "1 sheet (*)" -> 1 sheet
  s <- gsub("\\[1\\] sheet", "1 sheet", s)
  s <- gsub("1/[0-9] sheet", "1 sheet", s)
  s <- gsub(paste0("1 sheet \\(.{0,}\\)$"), "1 sheet", s)

  # Harmonize '* sheets'
  spl <- unlist(strsplit(s, ","))

  sheet.inds <- grep("sheet", spl)
  for (i in sheet.inds) {

    if (length(grep("^[0-9] sheet", s)) > 0) {
      n <- as.numeric(str_trim(unlist(strsplit(spl[[i]], "sheet"))[[1]]))
      spl[[i]] <- paste(n, "sheets", sep = " ")
    }

    if (length(grep("\\[^[0-9]|[a-z]\\] sheets", s)) > 0) {
      n <- as.numeric(as.roman(str_trim(gsub("\\[", "", gsub("\\]", "", unlist(strsplit(spl[[i]], "sheet"))[[1]])))))
      spl[[i]] <- paste(n, "sheets", sep = " ")
    }

    s <- paste(spl, collapse = ",")
    s <- gsub("1 sheets", "1 sheet", s)

  }

  # Table
  s <- gsub("folded genealogical table", "table", s)
  s <- gsub("folded table", "table", s)
  s <- gsub("fold\\. table", "table", s)
  s <- gsub("fold\\.tables", "tables", s)
  s <- gsub("table", "plate", s)
  s <- gsub("plate", "sheet", s)

  # Double
  s <- gsub("\\([0-9] double\\)", " ", s)
  s <- gsub("doubled", " ", s)
  s <- gsub("double", " ", s)
  s <- gsub("single", " ", s)

  # Harmonize broadside
  s <- gsub("broadside of ill.", "broadside", s)
  s <- gsub("broadside ([1] p.)", "broadside", s)
  s <- gsub("broadsheet", "broadside", s)
  s <- gsub("broadside", "sheet", s)

  # Quarto etc?
  # Set these to NA
  # and afterwards assign some estimated page count given for 
  # books of that size
  s <- gsub("^quarto$", "1 sheet", s) # 
  s <- gsub("^broadside$", "1 sheet", s) # 
  s <- gsub("^folios$", "1 sheet", s) # 
  s <- gsub("^folio$", "1 sheet", s) # 
  s <- gsub("^\\(fol.\\)$", "1 sheet", s) # 

  # maps count as normal pages
  s <- gsub("\\(map", "map", s)
  s <- gsub("map\\)", "map", s)
  s <- gsub("maps\\)", "map", s)
  s <- gsub("map8⁰", "map", s)
  s <- gsub("folded map", "map", s)
  s <- gsub("map", " 1p", s)

  # blank
  s <- gsub("\\[1 blank\\]", "[1]", s)

  s 

}


harmonize_ie <- function (s) {

  # Harmonize i.e.
  s <- gsub("i\\. e", " i.e", s)
  s <- gsub("\\[i\\.e", "  i.e", s)
  s <- gsub("\\[ie\\.", " i.e", s)
  s <- gsub("\\[ ie\\.", " i.e", s)
  s <- gsub("\\[ ie ", " i.e", s)
  s <- gsub("\\[ie ", " i.e", s)
  s <- gsub("\\,i\\.e", " i.e", s)
  s <- gsub("\\, i\\.e", " i.e", s)
  s <- gsub("i\\.e", " i.e.", s)
  s <- gsub("i\\.e\\.\\.", " i.e.", s)
  s <- gsub("i\\.e\\.\\,", " i.e.", s)
  s <- gsub("i\\.e\\.", " i.e ", s)
  s <- gsub("ie\\.", " i.e ", s)

  # "12 [i.e. 8 p.]" -> 12 i.e 8
  if (length(grep("\\[i.e ", s)) > 0) {
    s2 <- str_trim(unlist(strsplit(s, "\\[i.e "))[[2]])
    s2 <- pick_starting_numeric(s2)
    s <- gsub(paste("\\[i\\.e  ", s2, "\\]", sep = ""), paste("i\\.e", s2, " ", sep = ""), s)
    s <- gsub(paste("\\[i\\.e  ", s2, " p\\]", sep = ""), paste("i\\.e", s2, " ", sep = ""), s)
    s <- gsub(paste("\\[i\\.e  ", s2, " p\\.\\]", sep = ""), paste("i\\.e", s2, " ", sep = ""), s)
  }

  s <- condense_spaces(s)
  s <- gsub("p\\. i\\.e", " i.e", s)
  s <- gsub("i\\.e","i.e ",s)
  s <- condense_spaces(s)
  s

}


harmonize_romans <- function (s) {

  # Romans
  s <- gsub("leaf lxxxvij", "lxxxvij", s)
  s <- gsub("leaf C.xxxv", "C.xxxv", s)
  s <- gsub("CVXI", "CXVI", s) # Typo ?
  s <- gsub("c\\.lii", "clii", s) 
  s <- gsub("C\\.", "C", s) 
  s <- gsub("Cl\\.", "Cl", s) 
  s <- gsub("\\.\\]", "]", s) 
  s <- gsub("xxvii\\.", "xxvii", s) 

  # NOTE: in some systems lvii -> lvij etc. Handle this:
  s <- gsub("ij", "ii", s) 
  s <- gsub("xj", "xi", s) 
  s <- gsub("C\\.", "C", s) 

  s

}


harmonize_pages_specialcases <- function (s) {  

  # Remove some special cases manually		
  s <- gsub("Caption title; with a docket title that reads 'Memorial of the agent for the province of Massachussetts-Bay against a duty of 3d. per gallon on foreign molasses.'. - Dated in MS '9th February, 1764' and signed J. Mauduit.", NA, s)
  s <- gsub("in various pagings", "", s)
  s <- gsub("5 v. ; 42-43 cm \\(2⁰\\)", "5 v.;", s)
  s <- gsub("\\#\\,", ",", s)
  s <- gsub("\\(v.1: \\[72\\], 658, \\[32\\] p.; v.2: \\[144\\], 530, \\[14\\]; 237, \\[1\\] p.; v.3: \\[116\\], 465, \\[21\\]; 370, \\[2\\] p.\\) ;", "(v.1: [72], 658, [32] p.; v.2: [144], 530, [14], 237, [1] p.; v.3: [116], 465, [21], 370, [2] p.) ;", s)
  s <- gsub("2 v. \\(v.1: \\[8\\], 124 \\[i.e. 126\\], \\[1\\] leaves; 289, \\[1\\], \\[8\\], 22, \\[2\\], 518, \\[26\\] p., \\[2\\], 28 leaves, 115, \\[9\\] p.; v.2: \\[4\\], 291-659, 700-981, 990-1593, \\[1\\], 1593-1876, \\[104\\] p., folded plate\\) :", "2 v. (v.1: [8], 124 [i.e. 126], [1] leaves, 289, [1], [8], 22, [2], 518, [26] p., [2], 28 leaves, 115, [9] p.; v.2: [4], 291-659, 700-981, 990-1593, [1], 1593-1876, [104] p., folded plate) :", s)
  s <- gsub("2 v. \\(v.1: \\[8\\], 124 \\[i.e. 126\\], \\[1\\] leaves; 289, \\[1\\]; \\[8\\], 22, \\[2\\]; 518, \\[26\\] p.; \\[2\\], 28 leaves; 115, \\[9\\] p.; v.2: \\[4\\], 291-659, 700-981, 990-1593, \\[1\\], 1593-1876; \\[104\\] p., folded plate\\) :", "2 v. (v.1: [8], 124 [i.e. 126], [1] leaves, 289, [1], [8], 22, [2], 518, [26] p., [2], 28 leaves, 115, [9] p.; v.2: [4], 291-659, 700-981, 990-1593, [1], 1593-1876, [104] p., folded plate) :", s)
  s <- gsub("3 v. \\(vol. 1: \\[10\\], 250; \\[4\\], 202, \\[2\\] p.; vol. 2: 61, \\[13\\], 183, \\[1\\]; 421, 424-430, 436-438, 431-433, 439-445, 450-464, \\[56\\] p.; vol. 3: \\[8\\], 1080, 1080-1327, \\[2\\], 1332-1371, 1371-1421, 1490-1491, 1536-1555, 1574-1592, \\[62\\] p.\\) ;", "3 v. (vol. 1: [10], 250, [4], 202, [2] p.; vol. 2: 61, [13], 183, [1], 421, 424-430, 436-438, 431-433, 439-445, 450-464, [56] p.; vol. 3: [8], 1080, 1080-1327, [2], 1332-1371, 1371-1421, 1490-1491, 1536-1555, 1574-1592, [62] p.) ;", s)
  s <- gsub("v\\. ; 42 cm \\(2⁾", " ", s)

  # Caption title; with a docket title that reads 'Memorial of the
  # agent for the province of Massachussetts-Bay against a duty of
  # 3d. per gallon on foreign molasses.'. - Dated in MS '9th February,
  # 1764' and signed J. Mauduit.
  s[grep("Caption title", s)] <- NA
  
  # Add manually some missing commas
  s <- gsub("\\[8\\] 140 p\\.", "[8], 140", s)
  s <- gsub("\\[8\\] 182", "[8], 182", s)
  s <- gsub("\\[16\\] 240", "[16], 240", s)

  s
}


# A single instance of pages within commas
harmonize_pages_by_comma <- function (s) {

  # Harmonize '1 sheet'
  if (length(grep("1 sheet", s)) > 0 || s == "sheet") {
    s <- "1 sheet" 
  }

  # Harmonize '* sheets'
  if (length(grep("^[2-9] sheets", s)) > 0) {
    s <- paste(as.numeric(str_trim(unlist(strsplit(s, "sheet"))[[1]])), "sheets", sep = " ")
  }

  # Harmonize '1 broadside'
  if (length(grep("1 broadside", s)) > 0) {
    s <- "1 broadside" 
  }

  # 165-167 leaves -> 165-167
  if (length(grep("-", s))>0 && length(grep("leaves", s))>0) {
    s <- str_trim(gsub("leaves", "", s))
  }

  s <- polish_ie(s)

  # Convert plates to pages
  s <- plates2pages(s)

  # After plate operations handle p
  if (length(grep("plates", s)) == 0) {
    s <- gsub("pages", " ", s)
    s <- gsub("page", " ", s)
    s <- gsub("p\\.\\)", " ", s)
    s <- gsub("p$", " ", s)
    s <- gsub("p\\.]$", " ", s)
    s <- gsub(" p \\]$", " ", s)
    s <- gsub(" p\\]$", " ", s)
    #s <- gsub("p\\.", " ", s)
    #s <- gsub("p", " ", s)
  }
  # p66 -> 1
  if (length(grep("^p", s)) > 0 && length(grep("-", s)) == 0) {
    tmp <- as.numeric(str_trim(gsub("^p", "", s)))
    if (!is.na(tmp)) { s <- 1 }
  } else if (length(grep("^p", s)) > 0 && length(grep("-", s)) > 0) {
    s <- str_trim(gsub("^p", "", s))
  }

  # Handle some odd cases
  s <- gsub("a-m", " ", s)
  s <- trimming(s,n = 5)
  s <- condense_spaces(s)
  s[s == ""] <- NA

  s

}
