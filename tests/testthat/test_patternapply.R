library(patternapply)
context("Calling patternapply()")

inputStrings <- paste(letters, seq_along(letters), sep = "_")
patterns <- c("([[:digit:]]{2})", "([[:alpha:]])")
replacements <- list("\\1", "\\1")
patternapplyResult <- patternapply(inputStrings, patterns, replacements)
patternapplyResultVector <- patternapplyResult
attributes(patternapplyResultVector) <- NULL

test_that("Simple call returned correct results", {
  expect_equal(patternapplyResultVector, as.list(c(letters[1:9], 10:26)))
  expect_is(patternapplyResult, "replacement_list")
  expect_equal(attr(patternapplyResult, "col_names"), "match_1")
  expect_equal(attr(patternapplyResult, "match_index"),
               c(rep(2, 9), rep(1, 26-9)))
})

test_that("Bad inputs raise errors", {
  expect_error(patternapply(letters, 1, list("a")),
               "'patterns' must be a character vector")
  expect_error(patternapply(letters, "a", "a"),
               "'replacements' must be a list")
  expect_error(patternapply(letters, c("a", "b"), list(1, "a")),
               "'replacements' can only contain character vectors")
  expect_error(patternapply(letters, c("a", "b"), list("c")),
               "'patterns' and 'replacements' must have the same length")
})

test_that("Default arguments work", {
  expect_equal(patternapply(paste(1:20), c("^[0-9]$", "^[0-9]{2}$"))[1:20], 
               as.list(c(rep("1", 9), rep("2", 11))))
})

