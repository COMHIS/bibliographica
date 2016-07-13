context("Language")

test_that("Language recognition works correctly", {
  expect_equal(as.character(mark_languages("swe;eng")$language), "Swedish;English")
  expect_true(mark_languages("swe")$language.Swedish)
  expect_true(mark_languages("swe;")$language.Swedish)
  expect_true(mark_languages("Swedish")$language.Swedish)      
  expect_true(mark_languages("swe;eng")$language.Swedish)
  expect_true(mark_languages("swe;eng")$language.English)
  expect_true(is.na(unlist(mark_languages(c("swe;eng", "fin", "und", "lat;eng"))[3,"language"])))
  expect_equal(as.character(unlist(mark_languages(c("swe;eng", "fin", "und", "lat;eng"))[2,"language"])), "Finnish")
  expect_true(unlist(mark_languages(c("swe;eng", "fin", "und", "lat;eng"))[2,"language.Finnish"]))
  expect_true(unlist(mark_languages(c("swe;eng", "fin", "und", "lat;eng"))[4,"language.English"]))
  expect_true(unlist(mark_languages(c("swe;eng", "fin", "und", "lat;eng"))[4,"language.Latin"]))
  expect_true(!unlist(mark_languages(c("swe;eng", "fin", "und", "lat;eng"))[4,"language.Swedish"]))
  expect_true(mark_languages("swe;eng")[["language.Multiple languages"]])
  expect_true(!mark_languages(c("swe;eng", "lat"))[["language.Multiple languages"]][[2]])    

})