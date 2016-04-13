

context("Names")

test_that("First name picking ok", {
  expect_equal(unname(pick_firstname("Hobbes, Thomas", format = "last, first")), "Thomas")
  expect_equal(unname(pick_firstname("Thomas Hobbes", format = "first last")), "Thomas")  
})


test_that("Generic testing", {
  expect_equal(polish_author(" Candid Lover Of Truth.]")$names$full, "Candid Lover Of Truth")
})

test_that("Capitalization ok", {
  expect_equal(capitalize(c("thomas", "de vos"), "first.letter")[[1]], "Thomas")
  expect_equal(capitalize(c("thomas", "de vos"))[[2]], "De vos")
  expect_equal(capitalize(c("thomas", "de vos"), "all.words")[[2]], "De Vos")
})



