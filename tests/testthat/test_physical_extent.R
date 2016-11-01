context("Physical extent")

test_that("Page count is correct", {
  
  f <- system.file("extdata/tests_polish_physical_extent.csv", package = "bibliographica")
  synonymes <- read.csv(file=f, header=TRUE, sep="\t", encoding="UTF-8")
  for (i in 1:nrow(synonymes)) {
    test_clause <- as.character(synonymes$clause[i])
    test_result <- as.character(synonymes$result[i])
    test_field <- as.character(synonymes$field[i])
    if ((is.na(test_field)) || (test_field =="")) {test_field <- "pagecount"}
    if (is.na(synonymes$expected[i]) || (synonymes$expected[i] == "equal")) {
      expect_equal(as.character(polish_physical_extent(test_clause)[[test_field]]), test_result)
    }
    # print(c(i, as.character(polish_physical_extent(test_clause)[[test_field]]), test_result))
  }
    
  # TODO
  #2v.,table     2
  #2v.table      2
  #[4] s. 2:o.	4  
  #[1-3] 4-43 [44-45] 45-51 [po. 46-52] s.	52
  #4to [2] s., s. 113-111 [po. 128]     130
  #[08], 584 s. =(s. 566-84 opag.)=     592	8 + 584
  #[12], 1138 s. =(s. 1131-38 opag.)=   1150	12+1138
  #[16] , 376 s. =(s. 371-76 opag.)=.   394	16+376
  #[2], 13 [1] s.	   16	  2+13+1
  #2 vol.;(156, 166 s.)	  322	156+166
  #2 vol.;(130, 118 s.)=	  248	130+118
  #6 vol. i 2 (319 ; 246 s.) 565	319+246
  #vj,258s	 264	6+258 
  #[2] s., s. 65-82, IV kuvalehteä	28
  #1 kartasto ([166] s.)	166
  #1 sheet (2, [2] p.)	2 # = 1 sheet
  #[2] s., s. 129-143 [144-146]	20
  #VII s., s. 259-459	208
  #S. 14-28 ; s. 28-48	35
  #[4], [1] 2-23 [24] s.	28
  #[4], [1] 2-27 [28] s. (s. [28] tyhjä)	32
  #[4], [1] 2-30 32 [po. 31] [32] s.	34
  #[2] s., s. 161-176, [1] kuvalehti taitettuna	20
  #[2], 198 s. (s. 198 blank)     2          1 = 200
  #(4),44 bl.	48
  #38 uppsatser med särskild pag. (951)s.,1 portr.,19 pl.       38         1 = 991
  #(1)s.,s.53-552 552        1 = 501
  #[3] s., s. 497-740     743        1 = 247
  #3 vol.;(258, 308, 269 s.)      308        1 = 835
  #35[1] s.       2          3  = 36

# TODO
#[4] s. 2:o.	4  
#[1-3] 4-43 [44-45] 45-51 [po. 46-52] s.	52
#4to [2] s., s. 113-111 [po. 128]     130
#[08], 584 s. =(s. 566-84 opag.)=    592	8 + 584
#[16] , 376 s. =(s. 371-76 opag.)=.	394
#[2], 13 [1] s.	16
#2 vol.;(156, 166 s.)	322
#2 vol.;(130, 118 s.)=	248
#vj,258s	 264
#[2] s., s. 65-82, IV kuvalehteä	28
#6 vol. i 2 (319 ; 246 s.)	565
#1 kartasto ([166] s.)	166
#1 sheet (2, [2] p.)	2
#[2] s., s. 129-143 [144-146]	20
#VII s., s. 259-459	208
#S. 14-28 ; s. 28-48	35
#[4], [1] 2-23 [24] s.	28
#[4], [1] 2-27 [28] s. (s. [28] tyhjä)	32
#[4], [1] 2-30 32 [po. 31] [32] s.	34
#(4),44 bl.	48
#38 uppsatser med särskild pag. (951)s.,1 portr.,19 pl.	38
#3 vol.;(258, 308, 269 s.)	308
#[3] s., s. 497-740	743

  # Not urgent
  #494 s., Psaltarin kanssa 619 s.	619)
  #[15] s. neljässä jaksossa	15)
  #N. 500 s useina jaksoina	500)
  #160 spalter.	160)
  
  #  en osaa sanoa mikä on oikein, mutta nyt ei mene kuitenkaan oikein:
  #[02] , ii408, iii344 s.        2          4 = ???
  #6. vol.;(5-9 s./vol.)  9          1
  #___volymer     2          1
  #3 Teile 2          1
  #Varje nummer 4 s.      2          1
  #H. 1-3 i en vol.       2          1
  #17, 5 p.	22)
  #20, 2 p.	22)  
  
  # Not correct
  #   13 s., [2] taitettua karttalehteä -> 15		
  #   12 s., [2] taitettua karttalehteä -> 14		
  #   121 s., 6 kartor och 2 tab. -> 137		
  #   12 s., 1 kuval., och musikbilaga -> 16		 
  #   58 s. & omsl. -> 60		
  #   10s.,1 tab -> 12		
  #   [12] s. & omsl -> 14		
  
  
})


test_that("parts count is correct", {
  
  expect_equal(polish_physical_extent("2 parts")$parts, 2)
  expect_equal(polish_physical_extent("2 pts in 1 v. (viii, 332, 5, [1] p.)")$parts, 2)    
  
})

test_that("volume count is correct", {
  
  expect_equal(polish_physical_extent("2 pts in 1 v. (viii, 332, 5, [1] p.)")$volcount, 1)
  expect_equal(polish_physical_extent("v.7-9,plates")$volcount, 3)
  expect_true(is.na(polish_physical_extent("v")$volcount))
  expect_equal(polish_physical_extent("2 v")$volcount, 2)
  expect_equal(polish_physical_extent("2v")$volcount, 2)
  expect_equal(polish_physical_extent("5v.")$volcount, 5)
  expect_equal(polish_physical_extent("12v")$volcount, 12)
  expect_equal(polish_physical_extent("10v.")$volcount, 10)    
  expect_equal(polish_physical_extent("73 vols")$volcount, 73)
  expect_equal(polish_physical_extent("73 vol ")$volcount, 73)
  expect_equal(polish_physical_extent("73 v ")$volcount, 73)
  expect_equal(polish_physical_extent("73 v")$volcount, 73)
  expect_true(is.na(polish_physical_extent("73 parts, 2 pages")$volcount))
  expect_true(is.na(polish_physical_extent("73 pts,2 pages")$volcount)) 
  expect_true(is.na(polish_physical_extent("73 pts.,2 pages")$volcount))
  expect_equal(polish_physical_extent("1atlas")$volcount, 1)
  expect_true(is.na(polish_physical_extent("v, 5")$volcount)) # 5 + 5 pages, 1 volume
  expect_true(is.na(polish_physical_extent("v")$volcount))  
  expect_true(is.na(polish_physical_extent("v ;")$volcount))
  
  expect_equal(polish_physical_extent("[4] p. (p. [3] blank)")$pagecount, 4)
  expect_equal(polish_physical_extent("1 score (144 p.)")$pagecount, 144)
  expect_equal(polish_physical_extent("1 sheet ([2] p.), [18] leaves of plates")$pagecount, 38)

  # TODO

  # These are problematic. Too high page counts.

  # "4 v. (v.1: [36], 186, [10], 468, 479-642, 645-748, [24] p. (first leaf blank), [2] leaves of plates (1 folded); v.2: [4], 749-857, 860-1270, 1269-1860, [40] p.; v.3: [28], 668, 699-1006, 1009-1140, [68] p., [3] folded leaves of plates; v.4: [8], 1141-1234, 1237-1973 , [41] p. [2] folded leaves of plates)"
    # 4 voluuminen kirja jonka kokonaissivut juoksevat tuonne 1973, yhdessä volumessa ei ole tuhansia sivuja. Sitten nuo ylimaariset vielä siihen.

    # "7v.(2359[i.e.3359]p.),plates"
    # tuo i.e. kertoo 7 volumen sivujen kokonaisuuden (paitsi plates) eli 3359 sivua. Kirjaan on merkitty 2359 sivua joka on vaarin ja oikea luku on tuo 3359. Meidan koodi ei ilmeisesti ota nyt huomioon tuota. Pitaisi kuitenkin muistaakseni olla se sääntö, että i.e. tarkoittaa todellista sivujen maaraa. Tassa tapauksessa ei siis yhteenlaskua 2359+3359, vaan 3359+plates.


  # The current solution is to remove these after preprocessing as
  # suspiciously high cases. Better solution should be to fix this
  # already in the polish_physical_extent function.
  
  # "9 v. (v.1: [20], XII, xiii-lxxvj p., 1380 columns; v.2: [4] p., 1403-1586, 1585-1654, 1667-2796, 2805-2868 columns; v.3: [4] p., 2877-3852, 3851-4568 columns; v.4: [4] p., 4605-7064 columns; v.5: [4] p., 7125-8204 columns, [4] p., 8277-8788, 8805-9302 columns, [3] p., 9501-9634 columns, [1] p.; v.6: [4], lxviij p., 2108 columns; v.7: [4] p., 2113-5016 columns; v.8: [4] p., 648, 647-694, 697-1010, 1009-2190 columns; v.9: [4], 2215-4538, 4541-4548 columns, 4549-4566 p., 4567-4708 columns), [11] leaves of plates (mostly folded)" # Gives 36211 pages

  # "[2],iv,iv,380,377*-*378,473-502,553-568,395-430,513-536,441-464*379-*380,381-422,*521-*535,[1],v,[1]p." # Gives 464388 pages


})


test_that("volume count is correct", {
  expect_equal(polish_physical_extent("v.5")$volnumber, 5)
  expect_true(is.na(polish_physical_extent("v.7-9,plates")$volnumber))
})


# Summary of unit testing with testthat
# http://r-pkgs.had.co.nz/tests.html

# Create testthat directory and scripts:
# devtools::use_testthat()

# Test your package with 
# Ctrl/Cmd + Shift + T or devtools::test().

# Test options
#    is_true: Does the expression evaluate to TRUE?
#    is_false: Does the expression evaluate to FALSE?
#    is_a: Did the object inherit from a specified class?
#    equals: Is the expression equal within numerical tolerance to your expected value?
#    is_equivalent_to: Is the object equal up to attributes to your expected value?
#    is_identical_to: Is the object exactly equal to your expected value?
#    matches: Does a string match the specified regular expression?
#    prints_text: Does the text that’s printed match the specified regular expression?
#    throws_error: Does the expression raise an error?
#    takes_less_than: Does the expression take less than a specified number of seconds to run?

#test_that("floor_date works for different units", {
#  base <- as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC")
#  expect_equal(floor_date(base, "second")$pagecount, 
#    as.POSIXct("2009-08-03 12:01:59", tz = "UTC"))
#  expect_equal(floor_date(base, "minute")$pagecount
#}})