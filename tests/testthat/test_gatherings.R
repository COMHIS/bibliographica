context("Augmentation")

test_that("Augmenting values works correctly", {
  expect_equal(map_gatherings("8vo", from = "Standard", to = "Name"), "octavo")
  expect_equal(map_gatherings(c("8vo", "16mo"), from = "Standard", to = "Name")[[2]], "sextodecimo")
  expect_equal(as.character(map_gatherings(factor(c("8vo", "16mo"))), from = "Standard", to = "Name")[[2]], "sextodecimo")    
})

