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
