context("Dimension table")

test_that("dimension fill works correctly", {
  dimension.table <- dimension_table()
  #expect_equal(fill_dimensions(c(original="12to", gatherings="12to", width=13, height=NA), dimension.table)[["height"]], "19")
})