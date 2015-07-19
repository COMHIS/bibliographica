context("Dimension table")

test_that("dimension fill works correctly", {
  dimension.table <- dimension_table()
  expect_equal(fill_dimensions(c(original="12mo", gatherings="12mo", width=13, height=NA), dimension.table)[["height"]], "18")
})



test_that("dimension polish works correctly", {
  expect_equal(as.character(polish_dimensions("NA", fill = FALSE, dimtab = NULL)$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("2fo.;2fo.")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("2fo(3?).")$gatherings), "2fo")    
  expect_equal(as.character(polish_dimensions("2fo")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("4to;2fo")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("2fo;1to")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("4to-2fo")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("cm.4to")$gatherings), "4to")
  expect_equal(as.character(polish_dimensions("12mo.f")$gatherings), "12mo")    
 
})

