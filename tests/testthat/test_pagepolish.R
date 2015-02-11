context("Page count")

test_that("page count is correct", {
  expect_equal(unlist(polish_pages(c("1"))$estimated.pages), 1)
})

