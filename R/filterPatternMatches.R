#' Filter out all of the records that match an existing pattern.
#' 
#' @param X A character vector where matches are sought.
#' @param patterns A vector of regular expression patterns.
#'   
#' @return The subset of \code{X} that did not match any of the regexes in 
#'   \code{patterns}
#'   
#' @details This is meant to be useful when developing a vector of regexes to 
#'   apply to text. Regular expressions can be appended to \code{patterns}
#'   interactively until the edge cases are all covered, and then
#'   \code{patternapply()} can be deployed to extract data.
#' @export
filterPatternMatches <- function(X, patterns) {
  unmatchedRecords <- X
  for (pattern in patterns) {
    unmatchedRecords <- subset(unmatchedRecords, 
                               !grepl(pattern, unmatchedRecords))
  }
  return(unmatchedRecords)
}
