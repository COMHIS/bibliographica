test_that("collation formula evaluation", {
  x <- c(
    "π1 A`SUP`8`LO` B`SUP`6`LO`",
    "π1 A-F`SUP`8`LO`",
    "A-G`SUP`4`LO`",
    "A-@`SUP`4`LO`",
    "*`SUP`8`LO` A-2G`SUP`8`LO`",
    "2 instalments",
    "*-2*`SUP`12`LO` A-D`SUP`12`LO` E`SUP`6`LO`, `SUP`2`LO`A-H`SUP`12`LO` (`SUP`2`LO`A1 blank)",
    "A-2X`SUP`8/10`LO` 2Y`SUP`12`LO` [2Z]`SUP`2`LO`",
    "*`SUP`4`LO` A-7C`SUP`4`LO`",
    "✠`SUP`6`LO` A-3M`SUP`8`LO` 3N-3O`SUP`6`LO` `SUP`2`LO`A`SUP`6`LO` `SUP`2`LO`B-C`SUP`8`LO`",
    "a-s`SUP`8`LO` t`SUP`6`LO` A-3I`SUP`8`LO`",
    "`SUP`π`LO`a-c`SUP`4`LO` `SUP`π`LO`d`SUP`2`LO` A-4D`SUP`4`LO` `SUP`2`LO`A-2E`SUP`4`LO` `SUP`2`LO`2F`SUP`2`LO` a-2k`SUP`4`LO` 2l`SUP`2`LO`")
  y <- c(15, 49, 28, NA, 248, NA, 174, 410, 568, 504, 590, 554)
  expect_equal(eval_collation_formula(x), y)
})
