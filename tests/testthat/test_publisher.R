context("Publisher")

test_that("polish_publisher works correct", {
  expect_equal(as.character(polish_publisher("print Aboae")$name), "aboae")
  expect_equal(as.character(polish_publisher("Impressit Aboae")$name), "aboae")
  expect_equal(as.character(polish_publisher("Printed by John Dunlap")$name), "john dunlap")  
})

