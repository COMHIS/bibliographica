context("Dimension table")

test_that("dimension fill works correctly", {
  dimension.table <- dimension_table()
  expect_equal(fill_dimensions(c(original="12mo", gatherings="12mo", width=13, height=NA), dimension.table)[["height"]], "18")
})



test_that("dimension polish works correctly", {

  # escaped unicode character
  # \u00b0 = astemerkki ⁰

  #expect_equal(as.character(polish_dimensions("2\u00b0(3?)")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("2\u00b0(3?)")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("2fo(3?).")$gatherings), "2fo")    
  
  expect_equal(as.character(polish_dimensions("46 cm(2\u00b0)")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("29-40 cm. (4\u00b0; 2\u00b0)")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("4\u00b0. '")$gatherings), "4to")
  expect_equal(as.character(polish_dimensions("2\u00b0 (2 half-sheets)")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("1/2\u00b0; 2\u00b0.")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("2\u00b0; 1/2\u00b0.")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("1/2\u00b0? (34.5 x 16 cm.)")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("1/2\u00b0; 35.5 x 22.5 cm.")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("18 cm. (8vo; horizontal chain lines)")$gatherings), "8vo")
  expect_equal(as.character(polish_dimensions("1/2\u00b0(?)")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("40; 34; 42 cm (2\u00b0; 1/2\u00b0; 2\u00b0)")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("obl4\u00b0.")$gatherings), "4to")
  expect_equal(as.character(polish_dimensions("51-34 cm (2\u00b0-obl. 2\u00b0)")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("49 cm (1/2\u00b0, 2\u00b0)")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("42 cm.(2\u00b0)")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("20 cm. (4to?)")$gatherings), "4to")

  expect_equal(as.character(polish_dimensions("NA", fill = FALSE, dimtab = NULL)$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("2fo.;2fo.")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("2fo")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("4to;2fo")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("2fo;1to")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("4to-2fo")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("cm.4to")$gatherings), "4to")
  expect_equal(as.character(polish_dimensions("12mo.f")$gatherings), "12mo")
  expect_equal(as.character(polish_dimensions("4to.;4to")$gatherings), "4to")
  expect_equal(as.character(polish_dimensions("4to-4to")$gatherings), "4to")
  
  expect_equal(as.character(polish_dimensions("4\u00b0, 2\u00b0 and 1\u00b0.")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("31-44 cm (4\u00b0; 2\u00b0)")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("4\u00b0 and 8\u00b0.")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("4\u00b0; 2\u00b0.")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("4\u00b0; sm 2\u00b0; 2\u00b0 (30-51 cm.).")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("2\u00b0; 1\u00b0.")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("4\u00b0 and 2\u00b0.")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("2\u00b0; 4\u00b0.")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("2\u00b0; later, 1\u00b0.")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("44-55 cm (2\u00b0;1\u00b0)")$gatherings), "NA")

  expect_equal(as.character(polish_dimensions("4\u00b0; 43 cm (2\u00b0)")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("1/2.")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("1/2")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("8")$gatherings), "8vo")
  expect_equal(as.character(polish_dimensions("8\u00b0 in 4's.")$gatherings), "8vo")
  expect_equal(as.character(polish_dimensions("16\u00b0 in 8's.")$gatherings), "16mo")
  expect_equal(as.character(polish_dimensions("4\u00b0 in 8's.")$gatherings), "4to")
  expect_equal(as.character(polish_dimensions("12\u00b0 & 8\u00b0")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("21 cm. (4to and 8vo)")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("(4to and 8vo)")$gatherings), "NA")
  expect_equal(as.character(polish_dimensions("(fol)")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("2\u00b0; 32 x 20 cm.")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("Broadside. 35 x 24 cm.")$gatherings), "bs")
  expect_equal(as.character(polish_dimensions("32 cm. (fol))")$gatherings), "2fo")
  expect_equal(as.character(polish_dimensions("20 cm. (4to & 8vo)")$gatherings), "NA")

})

