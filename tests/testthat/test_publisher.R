context("Publisher")

test_that("polish_publisher works correct", {
  expect_equal(polish_publisher("print Aboae"), "aboae")
  expect_equal(polish_publisher("Impressit Aboae"), "aboae")
  expect_equal(polish_publisher("Printed by John Dunlap"), "john dunlap")  
})

