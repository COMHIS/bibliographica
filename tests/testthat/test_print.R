context("Harmonize print statements")

test_that("Print statements are harmonized correctly", {
  expect_equal(harmonize_print_statements("printed and sold by R. Marchbank")$name, "printed by R. Marchbank")
})

test_that("Print statements are removed correctly", {
  expect_equal(remove_print_statements("printed and sold by R. Marchbank"), "R. Marchbank")
  expect_equal(remove_print_statements("Printed in London"), "London")

  expect_equal(remove_print_statements("Tryckt i Vpsala"), "Vpsala")

})

