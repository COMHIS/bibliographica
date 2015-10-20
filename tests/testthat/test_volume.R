context("Volume count")

test_that("volume count is correct", {
  expect_equal(unname(polish_volumenumber("v.5")), 5)
  expect_true(is.na(unname(polish_volumenumber("v.7-9,plates"))))
})

test_that("remove volume functions correctly", {
  expect_true(is.na(unname(remove_volume_info("63 vols"))))
})

test_that("volume count is correct", {

  expect_equal(unname(polish_volumecount("v.7-9,plates")), 3)
  expect_equal(unname(polish_volumecount("v")), 1)
  expect_equal(unname(polish_volumecount("2 v")), 2)
  expect_equal(unname(polish_volumecount("2v")), 2)
  expect_equal(unname(polish_volumecount("5v.")), 5)
  expect_equal(unname(polish_volumecount("73 vols")), 73)
  expect_equal(unname(polish_volumecount("73 vol ")), 73)
  expect_equal(unname(polish_volumecount("73 v ")), 73)
  expect_equal(unname(polish_volumecount("73 v")), 73)
  expect_equal(unname(polish_volumecount("73 parts, 2 pages")), 1)
  expect_equal(unname(polish_volumecount("73 pts,2 pages")), 1) # Part is not volume
  expect_equal(unname(polish_volumecount("73 pts.,2 pages")), 1)
  expect_equal(unname(polish_volumecount("1atlas")), 1)
  expect_equal(unname(polish_volumecount("v, 5")), 1) # 5 + 5 pages, 1 volume
  expect_equal(unname(polish_volumecount("v")), 1)
  expect_equal(unname(polish_volumecount("v ;")), 1)
})

