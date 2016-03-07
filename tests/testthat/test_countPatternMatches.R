library(patternapply)
context("countPatternMatches")

test_that("countPatternMatches", {
  expect_equal(as.vector(countPatternMatches(paste(seq(0,99)), paste(seq(0,9)))), 
               c(10, rep(19, 9)))
  expect_equal(as.vector(countPatternMatches(letters, letters)), 
               rep(1, length(letters)))
  expect_equal(as.vector(countPatternMatches("abc", c("^a", "^b", "^."))),
               c(1, 0, 1))
})
