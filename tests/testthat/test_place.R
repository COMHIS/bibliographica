context("Harmonize place names")

test_that("Places are harmonized correctly", {

  expect_equal(polish_place("Sur l'imprimeà́ londres"), "London")
  expect_equal(polish_place("Te Philadelphia"), "Philadelphia Pa")
  expect_equal(polish_place("Imprinted in Aberdene"), "Aberdeen")

  expect_equal(polish_place("printe j stockholm"), "Stockholm")
  expect_equal(polish_place("Tryckt vthi Stockholm"), "Stockholm")
  expect_equal(polish_place("Imprimè [sic] a London"), "London")    

  expect_equal(polish_place("Tryckt i Vpsala"), "Uppsala")
  expect_equal(polish_place("A Londres : [s.n.]"), "London")
  expect_equal(polish_place("A Londres"), "London")
  expect_equal(polish_place("[A Londres"), "London")
  expect_equal(polish_place("A Londres."), "London")
  expect_equal(polish_place("A Londres; et se trouvent à Paris"), "London")
  expect_equal(polish_place("a Londres"), "London")
  expect_equal(polish_place("En Londres"), "London")
  expect_equal(polish_place("A. Londres"), "London")
  expect_equal(polish_place("Reprinted at Dublin"), "Dublin")
  expect_equal(polish_place("And re-printed at Dublin"), "Dublin")
  expect_equal(polish_place("[Reprinted at Dublin"), "Dublin")

  expect_equal(polish_place("In the Savoy :;Edinburgh"), "London")
  expect_equal(polish_place("In the Savoy"), "London")  

})


