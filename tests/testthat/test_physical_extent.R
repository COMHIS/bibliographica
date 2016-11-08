context("Physical extent")

test_that("Page count is correct", {
  
  f <- system.file("extdata/tests_polish_physical_extent.csv", package = "bibliographica")
  testcases <- read.csv(file=f, header=TRUE, sep="\t", encoding="UTF-8")
  for (i in 1:nrow(testcases)) {
    test_clause <- as.character(testcases$clause[i])
    test_result <- as.character(testcases$result[i])
    test_field <- as.character(testcases$field[i])
    if ((is.na(test_field)) || (test_field =="")) {test_field <- "pagecount"}
    if (is.na(testcases$expected[i]) || (testcases$expected[i] == "equal")) {
      expect_equal(as.character(polish_physical_extent(test_clause)[[test_field]]), test_result)
      #print(c(test_clause, as.character(polish_physical_extent(test_clause)[[test_field]]), test_result))
    }
  }
  
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