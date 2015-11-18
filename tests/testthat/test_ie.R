context("Polish ie")

test_that("Year polishing is correct", {

  expect_equal(handle_ie("ireland"), "ireland")
  expect_equal(handle_ie("i.e."), "i.e")  
  expect_equal(handle_ie("1642"), "1642")
  expect_equal(handle_ie("1641. [i.e. 1642]"), "1642")
  expect_equal(handle_ie("[1659]-1659 [i.e. 1660]", harmonize = TRUE), "1660")
  expect_equal(handle_ie("MDCXLI. [1641, i.e. 1642]"), "MDCXLI. [1642]")

})