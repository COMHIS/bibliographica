context("Harmonize place names")


test_that("Places are harmonized correctly", {
  f <- system.file("extdata/tests_place.csv", package = "bibliographica")
  synonymes <- read.csv(file=f, header=TRUE, sep="\t", encoding="UTF-8")
  for (i in 1:nrow(synonymes)) {
    test_clause <- as.character(synonymes$clause[i])
    test_result <- as.character(synonymes$result[i])
    if (is.na(synonymes$expected[i]) || (synonymes$expected[i] == "equal")) {
      expect_equal(polish_place(test_clause), test_result)
    }
  }
  
  
})