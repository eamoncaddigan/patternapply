library(patternapply)
context("Using generics")

inputStrings <- paste(letters, seq_along(letters), sep = "_")
patterns <- c("([[:digit:]]{2})",
              "([[:alpha:]])")
replacements <- list(c("\\1", "1"),
                     c("\\1", "2"))
patternapplyResult <- patternapply(inputStrings, patterns, replacements)
patternapplyResultMatrix <- as.matrix(patternapplyResult)
patternapplyResultDF <- as.data.frame(patternapplyResult)

test_that("Converting to matrix works", {
  expect_is(patternapplyResultMatrix, "matrix")
  expect_equal(ncol(patternapplyResultMatrix), 2)
})

test_that("Converting to data.frame works", {
  expect_is(patternapplyResultDF, "data.frame")
  expect_equal(ncol(patternapplyResultDF), 2)
})
