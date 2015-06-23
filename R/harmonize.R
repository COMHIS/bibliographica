
harmonize_pages <- function (x) {

  # Remove some special cases manually
  s <- harmonize_pages_specialcases(x)

  # Remove dimension info
  s <- remove_dimension(s)

  # ie harmonization (handle comma; otherwise ie handled later)
  s <- harmonize_ie(s)

  # Romans
  s <- harmonize_romans(s) 

  # Read the mapping table
  f <- system.file("extdata/harmonize_pages.csv", package = "bibliographica")
  harm <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))  
  # Harmonize
  for (i in 1:nrow(harm)) {
    s <- gsub(harm$synonyme[[i]], harm$name[[i]], s)
  }  

  s <- condense_spaces(s)

  # Remove endings
  for (i in 1:5) {
    s <- str_trim(remove_endings(s, c(" ", "\\.", "\\,", "\\;", "\\:")))
  }

  # Harmonize sheet, plate and table info
  s <- harmonize_sheets(s)

  # Pp. -> p etc.
  s <- harmonize_page_info(s)
  
  # Remove spaces around dashes and parentheses
  s <- gsub(" -", "-", s)
  s <- gsub("- ", "-", s)
  s <- str_trim(gsub("\\)", " ", gsub("\\(", " ", s)))
  s <- gsub(" \\(", ",(", s)
  s <- gsub("\\,\\,", ",", s)
  s <- gsub("^\\(", "", s)
  s <- gsub("\\)$", "", s)
  s <- condense_spaces(s)

  # Add commas
  # "[2] 4 p." -> "[2], 4 p."
  inds <- setdiff(1:length(s), grep("\\[i", s))
  s[inds] <- gsub(" \\[", "\\,[", s[inds])
  for (n in 0:9) {
    s <- gsub(paste("] ", n, sep = ""), paste("], ", n, sep = ""), s)
  }

  if (length(grep("^p[0-9]", s))) {
    s <- substr(s, 2, nchar(s))
  }

  s <- str_trim(gsub("^,", "", s))
  if (is.na(s) || s == "") { s <- NA }

  s

}



harmonize_page_info <- function (s) {

  f <- system.file("extdata/harmonize_page_info.csv", package = "bibliographica")
  harm <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))  

  # Harmonize
  for (i in 1:nrow(harm)) {
    s <- gsub(harm$synonyme[[i]], harm$name[[i]], s)
  }  


  s

}

harmonize_sheets <- function (s) {

  s <- gsub("[0-9]leaf", paste0(substr(s, 1, 1), " leaf"), s)

  # Read the mapping table
  f <- system.file("extdata/harmonize_sheets.csv", package = "bibliographica")
  harm <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))

  # Harmonize
  for (i in 1:nrow(harm)) {
    s <- gsub(harm$synonyme[[i]], harm$name[[i]], s)
  }  

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

  s 

}




harmonize_romans <- function (s) {

  # Read the mapping table
  f <- system.file("extdata/harmonize_romans.csv", package = "bibliographica")
  harm <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))

  # Harmonize
  for (i in 1:nrow(harm)) {
    s <- gsub(harm$synonyme[[i]], harm$name[[i]], s)
  }  

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

  # Harmonize i.e.  
  s <- handle_ie(s)

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
