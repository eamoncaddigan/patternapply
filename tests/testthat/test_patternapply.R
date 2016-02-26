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
  expect_equal(class(patternapplyResult), "replacement_list")
  expect_equal(attr(patternapplyResult, "col_names"), "match_1")
  expect_equal(attr(patternapplyResult, "match_index"), 
               c(rep(2, 9), rep(1, 26-9)))
})
