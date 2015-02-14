context("Page count")

test_that("page count is correct", {
  expect_equal(polish_pages("3")$estimated.pages[[1]], 3)
  expect_equal(polish_pages("[3]")$estimated.pages[[1]], 3)
  expect_equal(polish_pages("iii")$estimated.pages[[1]], 3)
  expect_equal(polish_pages("8,[28],37-88p.")$estimated.pages[[1]], 88)
  expect_equal(polish_pages("[2],1107-1217,[1]p.")$estimated.pages[[1]], 112)
  expect_equal(polish_pages("505-508")$estimated.pages[[1]], 4)
  expect_equal(polish_pages("35,8,9,16p")$estimated.pages[[1]], 35)
  expect_equal(polish_pages("1 sheet")$estimated.pages[[1]], 2)
  expect_equal(polish_pages("1 sheet ([1] page)")$estimated.pages[[1]], 2)
  expect_equal(polish_pages("1 sheet (2 pages)")$estimated.pages[[1]], 2)
  expect_equal(polish_pages("3 sheets (3 pages)")$estimated.pages[[1]], 6)
  expect_equal(polish_pages("xi,[1],309[i.e., 310],[2]p.")$estimated.pages[[1]], 324)
  expect_equal(polish_pages("[4], 31, 28-138, [2] p.")$estimated.pages[[1]], 144)
  expect_equal(polish_pages("vi,7-72p.")$estimated.pages[[1]], 72)
  expect_equal(polish_pages("[2], 58 p.")$estimated.pages[[1]], 60)
  expect_equal(polish_pages("vi,[1],8-67,[1]p.")$estimated.pages[[1]], 68)
  expect_equal(polish_pages("1 sheet")$estimated.pages[[1]], 2)
  expect_equal(polish_pages("2 sheets (versos blank)")$estimated.pages[[1]], 4)
  expect_equal(polish_pages("2 sheets")$estimated.pages[[1]], 4)
  expect_equal(polish_pages("[2], 6 p.")$estimated.pages[[1]], 8)
  expect_equal(polish_pages("6p.")$estimated.pages[[1]], 6)
  expect_equal(polish_pages("366 p.")$estimated.pages[[1]], 366)
  expect_equal(polish_pages("[2], 5, [1] p.")$estimated.pages[[1]], 8)
  expect_equal(polish_pages("1 broadside")$estimated.pages[[1]], 2)
  expect_equal(polish_pages("23,[1]p.")$estimated.pages[[1]], 24)
  expect_equal(polish_pages("[16] p.")$estimated.pages[[1]], 16)
  expect_equal(polish_pages("[2] leaves.")$estimated.pages[[1]], 4)
  expect_equal(polish_pages("v.")$estimated.pages[[1]], NA)
  expect_equal(polish_pages("[2]p.")$estimated.pages[[1]], 2)
  expect_equal(polish_pages("[3],vi-vii,[2],10-70,[2]p")$estimated.pages[[1]], 72)
  expect_equal(polish_pages("15 p.")$estimated.pages[[1]], 16)
  expect_equal(polish_pages("[2],VII,[1],90p.")$estimated.pages[[1]], 100)
  expect_equal(polish_pages("xviii,456,[2]p.")$estimated.pages[[1]], 476)
  expect_equal(polish_pages("2v.([14],242,[6],265-452,[12]p.")$estimated.pages[[1]], 484)
  expect_equal(polish_pages("262p.,plate")$estimated.pages[[1]], 264)
  expect_equal(polish_pages("[16],438[i.e.428],[52]p.")$estimated.pages[[1]], 496)
  expect_equal(polish_pages("127,[1]p.,plates")$estimated.pages[[1]], 132)
  expect_equal(polish_pages("vi,[1],iv-vii,[27],680,[24]p.")$estimated.pages[[1]], 739)
  expect_equal(polish_pages("[3],6-31,[1]p.")$estimated.pages[[1]], 30)
  expect_equal(polish_pages("[2],xxxv,[3],106,[26],286,[30]p.,plates,table")$estimated.pages[[1]], 388)
  expect_equal(polish_pages("v.4 ([8],418,[30]p.),plate")$estimated.pages[[1]], 458)
  expect_equal(polish_pages("9,[17]p.")$estimated.pages[[1]], 26)
  expect_equal(polish_pages("lxxiip.")$estimated.pages[[1]], 22)
  expect_equal(polish_pages("Pp.1-190p.,table")$estimated.pages[[1]], 192)
  expect_equal(polish_pages("24,24p.")$estimated.pages[[1]], 24)
  expect_equal(polish_pages("110,[1],4-60p.")$estimated.pages[[1]], 111)
  expect_equal(polish_pages("2v.(CLI,[1],800p.)")$estimated.pages[[1]], 952)
  expect_equal(polish_pages("72p.,ill.")$estimated.pages[[1]], 72)
  expect_equal(polish_pages("32,[16],33-56p.")$estimated.pages[[1]], 72)
  expect_equal(polish_pages(sum(unlist(polish_pages("[4],56,65-176;45,[1]p.")$estimated.pages)), 226)
  expect_equal(polish_pages("[3],vi-vii,[3],98p.")$estimated.pages[[1]], 111)
  expect_equal(polish_pages("vi,9-74p.")$estimated.pages[[1]], 72)
  expect_equal(polish_pages("[4],53,[13]p.,fold.tables")$estimated.pages[[1]], 72)
  expect_equal(polish_pages("xvi,3-46,[2]p.")$estimated.pages[[1]], 62)
  expect_equal(polish_pages("pp. 145-148")$estimated.pages[[1]], 4)
  expect_equal(polish_pages("pp. [45]-48")$estimated.pages[[1]], 4)
  expect_equal(polish_pages("p. [2], 129-142")$estimated.pages[[1]], 16)
  expect_equal(polish_pages("[12], 155-923, [47] p.")$estimated.pages[[1]], 828)
  expect_equal(polish_pages("48 leaves")$estimated.pages[[1]], 96)
  expect_equal(polish_pages("Ff. [8], 1-209, 209-213, 213-219")$estimated.pages[[1]], 454)
  expect_equal(polish_pages("122-126 p.")$estimated.pages[[1]], 5)
  expect_equal(polish_pages("p. 66")$estimated.pages[[1]], 1)
  expect_equal(polish_pages("[5], 4 [i.e. 6]-8 p.")$estimated.pages[[1]], 8)
  expect_equal(polish_pages("[2], 169-182 [i.e. 14] p.")$estimated.pages[[1]], 16)
  expect_equal(polish_pages("pp. 49-56")$estimated.pages[[1]], 8)	       
  expect_equal(polish_pages("[2], 3-58 p.")$estimated.pages[[1]], 58)
  expect_equal(polish_pages("[2], 283-287, [1] p.")$estimated.pages[[1]], 10)
  expect_equal(polish_pages("p. 237 [i.e. 245]-247, [1]")$estimated.pages[[1]], 4)
  expect_equal(polish_pages("1679 p. in various pagings.")$estimated.pages[[1]], 1679)
  expect_equal(polish_pages("iii-xxiv, 118, [2] p.")$estimated.pages[[1]], 142)
  expect_equal(polish_pages("[4], 31, 28-138, [2] p.")$estimated.pages[[1]], 144)
  expect_equal(polish_pages("[3], 4-8, p. 7, 10-11, 10-13, [1] p.")$estimated.pages[[1]], 16)

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
#  expect_equal(floor_date(base, "second"), 
#    as.POSIXct("2009-08-03 12:01:59", tz = "UTC"))
#  expect_equal(floor_date(base, "minute"), 
#}})