context("Mapping tables")

test_that("Region to country mappings are correct", {
  map <- region2country()
  expect_equal(as.character(get_country("Berlin", map)$country), "Germany")
  expect_equal(as.character(get_country(c("Berlin", "Munich", "Amsterdam"), map)$country[[3]]), "Netherlands")  
})

