context("Mapping tables")

test_that("Region to country mappings are correct", {
  expect_equal(as.character(get_country("Berlin")$country), "Germany")
  expect_equal(as.character(get_country("Altmore")$country), "Northern Ireland")  
  expect_equal(as.character(get_country(c("Berlin", "Munich", "Amsterdam"))$country[[3]]), "Netherlands")  
})



test_that("Country mappings are not ambiguous", {
  expect_equal(length(test_ambiguous_country()), 0)
  expect_equal(length(test_ambiguous_place()), 0)    
})

