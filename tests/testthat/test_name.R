

context("Names")

test_that("First name picking ok", {
  expect_equal(unname(pick_firstname("Hobbes, Thomas", format = "last, first")), "Thomas")
  expect_equal(unname(pick_firstname("Hobbes, Thomas", format = "last, first", keep.single = TRUE)), "Thomas")  
  expect_equal(unname(pick_firstname("Thomas Hobbes", format = "first last")), "Thomas")


  expect_equal(unname(pick_firstname("Thomas Hobbes", format = "first last", keep.single = TRUE)), "Thomas")
  expect_equal(unname(pick_firstname("Thomas", format = "last, first", keep.single = TRUE)), "Thomas")

  # Single occurrence always counted as first name when keep.single is true
  expect_equal(unname(pick_firstname("Hobbes", format = "last, first", keep.single = TRUE)), "Hobbes")    

  
})


test_that("Generic testing", {
  expect_equal(polish_author(" Candid Lover Of Truth.]"), "Candid Lover Of Truth")
})

test_that("Capitalization ok", {
  expect_equal(capitalize(c("thomas", "de vos"), "first.letter")[[1]], "Thomas")
  expect_equal(capitalize(c("thomas", "de vos"))[[2]], "De vos")
  expect_equal(capitalize(c("thomas", "de vos"), "all.words")[[2]], "De Vos")
})



