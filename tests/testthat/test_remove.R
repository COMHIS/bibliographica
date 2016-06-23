context("Term removal")

test_that("Term removal work correctly", {

  expect_equal(remove_terms("check anno", "anno", c("begin", "middle", "end")), "check")
  expect_equal(remove_terms("check anno", "anno", "full"), "check")
  expect_equal(remove_terms("check anno", "anno", "all"), "check")    

})

