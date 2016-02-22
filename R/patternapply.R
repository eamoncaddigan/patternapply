
#' Iteratively try patterns against a character vector.
#'
#' @param X A character vector where matches are sought.
#' @param patterns A vector of regular expression patterns.
#' @param replacements A vector of replacement information, must match the
#'   length of \code(patterns). This can either be a character vector or list of
#'   character vectors. This can include backreferences "\1" to "\9" to
#'   parenthesized subexpressions of the corresponding pattern.
#'
#' @return A vector of replacements. Matches the format of \code(replacements).
patternapply <- function(X, patterns,
                         replacements = paste(seq_along(patterns))) {

  # Keep track of which records have already been matched to a pattern.
  matchFalses <- rep(FALSE, length(X))
  matchedAlready <- matchFalses

  for (pattern in patterns) {
    # Match the pattern to the
    matchedIndices <- regexec(pattern, X[!matchedAlready])

    # Find all the places where matches occurred.
    matches <- vapply(matchedIndices, `[`, integer(1), 1) != -1
    matchedStrings <- regmatches(X, matchedIndices)

    # Where are new matches?
    matchedHere <- matchFalses
    matchedHere[!matchedAlready] <- vapply(matchedIndices, `[`, integer(1), 1) != -1

    # Fill in the data for the new matches.
    artists[matchedHere, commonCols] <- bioData[bioData$is_match, commonCols]

    # Update the list of matched rows.
    matchedAlready <- matchedAlready | matchedHere
  }
}
