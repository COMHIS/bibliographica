context("Augmentation")

test_that("Augmenting values works correctly", {
  expect_equal(guess_missing_entries(c("Tom", "Tom", "Pete", "Pete", "Pete"), c(1, NA, 2, 3, NA))[2, "values"], "1")
  expect_true(is.na(guess_missing_entries(c("Tom", "Tom", "Pete", "Pete", "Pete"), c(1, NA, 2, 3, NA))[5, "values"]))
})