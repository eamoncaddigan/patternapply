library(patternapply)
context("filterPatternMatches")

test_that("filterPatternMatches", {
  expect_equal(filterPatternMatches(paste(seq(0,99)), "[1-9]$"),
               paste(seq(0, 99, by=10)))
})
