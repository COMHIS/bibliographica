context("Harmonize print statements")

test_that("Print statements are harmonized correctly", {
  expect_equal(harmonize_print_statements("printed and sold by R. Marchbank")$name, "printed by r. marchbank")
})

