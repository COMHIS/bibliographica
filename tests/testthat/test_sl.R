context("Polish sl")

test_that("Year polishing is correct", {

  expect_true(is.na(remove_sl("s. l.")))
  expect_true(is.na(remove_sl("s.n.")))
  expect_true(is.na(remove_sl("s. a")))
  expect_true(is.na(remove_sl("s i.")))
  expect_true(is.na(remove_sl("n l")))
  expect_true(is.na(remove_sl("sl.")))
  expect_true(is.na(remove_sl("[s. n.]")))
  expect_true(is.na(remove_sl("[s.n.?]")))      

})