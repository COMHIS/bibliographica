context("Gender")

test_that("Gender assignments", {
  data(gendermap)
  expect_equal(get_gender("aapo", gendermap), "male")
  expect_equal(get_gender("zurildia", gendermap), "female")
  expect_equal(get_gender("leo", gendermap), "ambiguous")    
})

