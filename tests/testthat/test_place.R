context("Harmonize place names")

test_that("Places are harmonized correctly", {
  expect_equal(polish_place("Tryckt i Vpsala"), "Uppsala")
})

