context("Dimension table")

test_that("dimension fill works correctly", {
  dimension.table <- dimension_table()
  expect_equal(fill_dimensions(c(original="12mo", gatherings="12mo", width=13, height=NA), dimension.table)[["height"]], "20")
})


test_that("dimension tables are valid", {
  
  dt <- dimension_table()
  ss <- sheet_sizes()  
  gt <- gatherings_table()
  
  expect_true(all(ss$gatherings %in% gt$Standard))
  expect_true(all(setdiff(colnames(dt), c("height", "NA")) %in% gt$Standard))
  
})


test_that("dimension polish works correctly", {
  
  f <- system.file("extdata/tests_dimension_polish.csv")
  synonymes <- read.csv(file=f, header=TRUE, sep="\t", encoding="UTF-8")
  for (i in 1:nrow(synonymes)) {
    test_clause <- as.character(synonymes$clause[i])
    test_result <- as.character(synonymes$result[i])
    if (is.na(test_result)) {test_result <- "NA"}
    test_field <- as.character(synonymes$field[i])
    if ((is.na(test_field)) || (test_field =="")) {test_field <- "gatherings"}
    if (is.na(synonymes$expected[i]) || (synonymes$expected[i] == "equal")) {
      expect_equal(as.character(polish_dimensions(test_clause)[[test_field]]), test_result)
    }
    # Missing from csv:
    # expect_equal(as.character(polish_dimensions("NA", fill = FALSE, dimtab = NULL)$gatherings), "NA")
  }
  
  
})


test_that("fennica dimensions", {
  
  f <- system.file("extdata/tests_dimension_polish_fennica.csv")
  synonymes <- read.csv(file=f, header=TRUE, sep="\t", encoding="UTF-8")
  for (i in 1:nrow(synonymes)) {
    test_clause <- as.character(synonymes$clause[i])
    test_result <- as.character(synonymes$result[i])
    if (is.na(test_result)) {test_result <- "NA"}
    test_field <- as.character(synonymes$field[i])
    test_fill <- as.character((synonymes$fill[i]))
    
    # This appears to be completely irrelevant?
    if (test_fill=="TRUE") {test_fill <- TRUE} else {test_fill <- FALSE}
    if ((is.na(test_field)) || (test_field =="")) {test_field <- "gatherings"}
    if (is.na(synonymes$expected[i]) || (synonymes$expected[i] == "equal")) {
      expect_equal(as.character(polish_dimensions(test_clause)[[test_field]]), test_result)
    }
  }
  # Take care later
  #expect_equal(as.character(polish_dimensions("kotelo 48 x 56 cm.")$gatherings), 4)
  #expect_equal(as.character(polish_dimensions("obl.1to[-1/2to].")$gatherings), 4)
  #expect_equal(as.character(polish_dimensions("12to(=8to")$gatherings), 4)
  #expect_equal(as.character(polish_dimensions("2to(4to)")$gatherings), 4)
  #expect_equal(as.character(polish_dimensions("1/2to[1to].")$gatherings), 4)
  #expect_equal(as.character(polish_dimensions("4to[8to].")$gatherings), NA)
  #expect_equal(as.character(polish_dimensions("8to[16to].")$gatherings), NA)
  #expect_equal(as.character(polish_dimensions("obl.1to[1/2to].")$gatherings), 4)
  #expect_equal(as.character(polish_dimensions("obl.1to[1/2to.")$gatherings), 4)
  #expect_equal(as.character(polish_dimensions("obl.1to1/2to].")$gatherings), 4)
  
})


