context("Physical extent")

test_that("Page count is correct", {

  f <- "~/GitHub/bibliographica/tests/polish_physical_extent_tests.csv"
  synonymes <- read.csv(file=f, header=TRUE, sep="\t", encoding="UTF-8")
  for (i in 1:nrow(synonymes)) {
    test_clause <- as.character(synonymes$clause[i])
    test_result <- as.character(synonymes$result[i])
    test_field <- as.character(synonymes$field[i])
    if ((is.na(test_field)) || (test_field =="")) {test_field <- "pagecount"}
    if (is.na(synonymes$expected[i]) || (synonymes$expected[i] == "equal")) {
      expect_equal(as.character(polish_physical_extent(test_clause)[[test_field]]), test_result)
    }
  }


# TODO
#  expect_equal(polish_physical_extent("121 s., 6 kartor och 2 tab.")$pagecount, 121)
#  expect_equal(polish_physical_extent("11 s., 1 karta")$pagecount, 11)  
#  expect_equal(polish_physical_extent("12 s., 1 kuval., och musikbilaga")$pagecount, 12)
#  expect_equal(polish_physical_extent("17 s., [33] kuvas., [8] kuvalehteä taitettuna")$pagecount, 33)
#  expect_equal(polish_physical_extent("[2] s., s. 65-82, IV kuvalehteä")$pagecount, 28)
#  expect_equal(polish_physical_extent("[4], 87 s., 8 karttalehteä")$pagecount, 91)

#  expect_equal(polish_physical_extent("5 vol. i 6     2          4 = EI MUKAAN
#  expect_equal(polish_physical_extent("6 vol. i 7     2          4 = EI MUKAAN
#  expect_equal(polish_physical_extent("5 delar 2          3 = EI MUKAAN

#  expect_equal(polish_physical_extent("58 s. & omsl.")$pagecount, 2)  
#  expect_equal(polish_physical_extent("1 kartasto ([166] s.)")$pagecount, 166)
#  expect_equal(polish_physical_extent("1 sheet (2, [2] p.)")$pagecount, 2) # = 1 sheet
#  expect_equal(polish_physical_extent("[2] s., s. 129-143 [144-146]")$pagecount, 20)
#  expect_equal(polish_physical_extent("VII s., s. 259-459")$pagecount, 208)
#  expect_equal(polish_physical_extent("S. 14-28 ; s. 28-48")$pagecount, 35)
#  expect_equal(polish_physical_extent("[4], [1] 2-23 [24] s.")$pagecount, 28)
#  expect_equal(polish_physical_extent("[4], [1] 2-27 [28] s. (s. [28] tyhjä)")$pagecount, 32)
#  expect_equal(polish_physical_extent("[4], [1] 2-30 32 [po. 31] [32] s.")$pagecount, 34)
#  expect_equal(polish_physical_extent("[2] s., s. 161-176, [1] kuvalehti taitettuna")$pagecount, 20)  
#  expect_equal(polish_physical_extent("[2], 198 s. (s. 198 blank)     2          1 = 200
#  expect_equal(polish_physical_extent("46,(2)s.,2 kartor på omsl.     46         3 = 48
#  expect_equal(polish_physical_extent("10s.,1 tab.    10         2 = 12
#  expect_equal(polish_physical_extent("(4),44 bl.")$pagecount, 48)
#  expect_equal(polish_physical_extent("38 uppsatser med särskild pag. (951)s.,1 portr.,19 pl.       38         1 = 991
#  expect_equal(polish_physical_extent("(1)s.,s.53-552 552        1 = 501
#  expect_equal(polish_physical_extent("[3] s., s. 497-740     743        1 = 247
#  expect_equal(polish_physical_extent("3 vol.;(258, 308, 269 s.)      308        1 = 835
#  expect_equal(polish_physical_extent("35[1] s.       2          3  = 36
#  expect_equal(polish_physical_extent("[52] plates between [58] blank leaves")$pagecount, 220)

# Not urgent
#  expect_equal(polish_physical_extent("494 s., Psaltarin kanssa 619 s.")$pagecount, 619)
#  expect_equal(polish_physical_extent("[15] s. neljässä jaksossa")$pagecount, 15)
#  expect_equal(polish_physical_extent("N. 500 s useina jaksoina")$pagecount, 500)
#  expect_equal(polish_physical_extent("160 spalter.")$pagecount, 160)

#  en osaa sanoa mikä on oikein, mutta nyt ei mene kuitenkaan oikein:
#  expect_equal(polish_physical_extent("[02] , ii408, iii344 s.        2          4 = ???
#  expect_equal(polish_physical_extent("6. vol.;(5-9 s./vol.)  9          1
#  expect_equal(polish_physical_extent("___volymer     2          1
#  expect_equal(polish_physical_extent("3 Teile 2          1
#  expect_equal(polish_physical_extent("Varje nummer 4 s.      2          1
#  expect_equal(polish_physical_extent("H. 1-3 i en vol.       2          1
#  expect_equal(polish_physical_extent("17, 5 p.")$pagecount, 22)
#  expect_equal(polish_physical_extent("20, 2 p.")$pagecount, 22)  



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
