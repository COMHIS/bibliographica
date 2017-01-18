context("Physical extent")

test_that("Page count is correct", {
  
  f <- system.file("extdata/tests_polish_physical_extent.csv", package = "bibliographica")
  testcases <- read.csv(file=f, header=TRUE, sep="|", encoding="UTF-8")
  for (i in 1:nrow(testcases)) {
    test_clause <- as.character(testcases$clause[i])
    test_result <- as.character(testcases$result[i])
    test_field <- as.character(testcases$field[i])
    if ((is.na(test_field)) || (test_field =="")) {test_field <- "pagecount"}
    if (is.na(testcases$expected[i]) || (testcases$expected[i] == "equal")) {
      expect_equal(as.character(polish_physical_extent(test_clause)[[test_field]]), test_result)
      # print(c(test_clause, as.character(polish_physical_extent(test_clause)[[test_field]]), test_result))
    }
  }
  
})


