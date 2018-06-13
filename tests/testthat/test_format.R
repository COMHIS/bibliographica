context("Time period conversions")

test_that("Time period conversions works correctly", {

  expect_equal(format_period("[1, 5)"), "1-4")
  expect_equal(format_period("[1, 5]"), "1-5")
  expect_equal(format_period("(1, 5]"), "2-5")
  expect_equal(format_period("(1, 5)"), "2-4")
  expect_equal(format_period("(1, 3)"), "2")  

  expect_equal(century(1729), 1700)
  expect_equal(century(1700), 1700)
  expect_equal(century(1799), 1700)

  expect_equal(decade(1729), 1720)
  expect_equal(decade(1700), 1700)
  expect_equal(decade(1799), 1790)    

})

