context("Harmonize print statements")

test_that("Print statements are harmonized correctly", {
  expect_equal(harmonize_print_statements("printed and sold by R. Marchbank")$name, "printed by r. marchbank")
})

test_that("Print statements are removed correctly", {
  expect_equal(remove_print_statements("printed and sold by R. Marchbank"), "r. marchbank")
  expect_equal(remove_print_statements("Printed in London"), "london")
  expect_equal(remove_print_statements("Tryckt i Vpsala"), "vpsala")

})

