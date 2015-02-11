context("Page count")

test_that("page count is correct", {
  expect_equal(polish_pages("3")$estimated.pages[[1]], 3)
  expect_equal(polish_pages("[3]")$estimated.pages[[1]], 3)
  expect_equal(polish_pages("iii")$estimated.pages[[1]], 3)
  expect_equal(polish_pages("[1], 3-70, [2]")$estimated.pages[[1]], 70)
  expect_equal(polish_pages("8,[28],37-88p.")$estimated.pages[[1]], 88)
  expect_equal(polish_pages("[2],1107-1217,[1]p.")$estimated.pages[[1]], 112)
  expect_equal(polish_pages("505-508")$estimated.pages[[1]], 4)
})


