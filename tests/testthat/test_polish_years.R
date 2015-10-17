context("Polish years")

test_that("Year polishing is correct", {
  
  expect_equal(polish_years("1741-1810.")$from, 1741)    
  expect_equal(polish_years("1741-1810.")$till, 1810)
  
  expect_equal(polish_years("1798. (price one dollar)")$from, 1798)
  expect_equal(polish_years("1776.")$from, 1776)  
  expect_equal(polish_years("[1768.]")$from, 1768)
  expect_equal(polish_years("[-1768]")$till, 1768)
  expect_equal(polish_years("-1776.")$till, 1776)
  expect_equal(polish_years("1524]")$from, 1524)
  expect_equal(polish_years("1524?]")$from, 1524)
  expect_equal(polish_years("[1524?]")$from, 1524)
  expect_equal(polish_years("--1524.---")$from, 1524)
  expect_equal(polish_years("[ca. 1618]")$from, 1618)  

  expect_equal(polish_years("MDCCLXVIII. [1768]")$from, 1768)
  expect_equal(polish_years("MDCCLXVIII. 1768")$from, 1768)

  expect_equal(polish_years("active 1781.")$till, 1781)
  expect_true(is.na(polish_years("-1776.")$from))

  expect_equal(polish_years("1741?-1821.")$from, 1741)
  expect_equal(polish_years("1741?-1821.")$till, 1821)
  
  expect_equal(polish_years("approximately 19 B.C.-approximately 30 A.D.")$from, -19)
  expect_equal(polish_years("approximately 19 B.C.-approximately 30 A.D.")$till, 30)
  
  expect_equal(polish_years("1650 or 1651-1705.")$from, 1651)
  expect_equal(polish_years("1650 or 1651-1705.")$till, 1705)
  expect_equal(polish_years("[1524-1528]")$from, 1524)
  expect_equal(polish_years("[1524-1528]")$till, 1528)
  
  expect_equal(polish_years("[between 1790 and 1800?]")$from, 1790)
  expect_equal(polish_years("[between 1790 and 1800?]")$till, 1800)

  expect_true(is.na(polish_years("active 17th century.")$from))
  expect_true(is.na(polish_years("active 17th century.")$till))

})


