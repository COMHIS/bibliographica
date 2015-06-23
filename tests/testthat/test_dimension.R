context("Dimension table")

test_that("dimension fill works correctly", {
  dimension.table <- dimension_table()
  expect_equal(fill_dimensions(c(original="12mo", gatherings="12mo", width=13, height=NA), dimension.table)[["height"]], "18")

  expect_equal(as.character(polish_dimensions("2fo")$gatherings), "2fo")

})