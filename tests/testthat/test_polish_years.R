context("Polish years")

test_that("Year polishing is correct", {
  
  expect_equal(polish_years("1741-1810.")$start, 1741)    
  expect_equal(polish_years("1741-1810.")$end, 1810)

  # These do not work yet
  expect_equal(polish_years("active 1781.")$end, 1781)
  expect_equal(polish_years("-1776.")$start, NA)
  expect_equal(polish_years("-1776.")$end, 1776)
  expect_equal(polish_years("1741?-1821.")$start, 1741)
  expect_equal(polish_years("1741?-1821.")$end, 1821)
  expect_equal(polish_years("approximately 19 B.C.-approximately 30 A.D.")$start, -19)
  expect_equal(polish_years("approximately 19 B.C.-approximately 30 A.D.")$end, 30)
  expect_equal(polish_years("1650 or 1651-1705.")$start, 1651)
  expect_equal(polish_years("1650 or 1651-1705.")$end, 1705)
  expect_equal(polish_years("active 17th century.")$start, 1600)
  expect_equal(polish_years("active 17th century.")$end, 1699)  

})


