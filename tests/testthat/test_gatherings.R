context("Gatherings")

test_that("Augmenting values works correctly", {
<<<<<<< HEAD
  expect_equal(map_gatherings("8vo", from = "Standard", to = "Name"), "Octavo")
  expect_equal(map_gatherings(c("8vo", "16mo"), from = "Standard", to = "Name")[[2]], "Sextodecimo")
  expect_equal(as.character(map_gatherings(factor(c("8vo", "16mo"))), from = "Standard", to = "Name")[[2]], "Sextodecimo")    
||||||| merged common ancestors
  expect_equal(map_gatherings("8vo", from = "Standard", to = "Name"), "octavo")
  expect_equal(map_gatherings(c("8vo", "16mo"), from = "Standard", to = "Name")[[2]], "sextodecimo")
  expect_equal(as.character(map_gatherings(factor(c("8vo", "16mo"))), from = "Standard", to = "Name")[[2]], "sextodecimo")    
=======
  expect_equal(map_gatherings("8vo", from = "Standard", to = "Name"), "Octavo")
  expect_equal(map_gatherings(c("8vo", "16mo"), from = "Standard", to = "Name")[[2]], "Sextodecimo")
  expect_equal(as.character(map_gatherings(factor(c("8vo", "16mo"))), from = "Standard", to = "Name")[[2]], "Sextodecimo")
  expect_equal(as.character(levels(order_gatherings(factor(c("2fo", "1to", "8vo")))))[[3]], "8vo")
  expect_equal(as.character(levels(order_gatherings(factor(c("folio", "sheet", "octavo")))))[[1]], "sheet")
  expect_equal(as.character(levels(order_gatherings(factor(c("Folio", "Sheet", "Octavo")))))[[1]], "Sheet")
  expect_equal(order_gatherings(c("1to", NA, "16mo"))[[3]], "16mo")
  expect_true(is.na(order_gatherings(c("1to", NA, "16mo"))[[2]]))            
>>>>>>> be368d57b4b6bfb6bdd0aa87f8221ac71a663880
})

