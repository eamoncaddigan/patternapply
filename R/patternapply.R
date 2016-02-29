#' patternappy: A package for attempting to apply patterns to a vector.
#' 
#' This package contains the function \code{patternapply()}, which iteratively 
#' tries to match a list of regular expressions to a vector and returns the
#' associated replacement vector. This is useful for turning a column of text
#' data with different formats into a matrix or data frame. S3 generics are also
#' provided to assist with this.
#' 
#' @docType package
#' @name patternapply
NULL


#' Iteratively try patterns against a character vector.
#'
#' @param X A character vector where matches are sought.
#' @param patterns A vector of regular expression patterns.
#' @param replacements A list of replacement information, which must match the 
#'   length of \code{patterns}. Each element must be a character vector. This
#'   can include backreferences "\1" to "\9" to parenthesized subexpressions of
#'   the corresponding pattern.
#'
#' @return A list of replacement vectors with class "replacement_list".
#' 
#' @rdname patternapply_fun
patternapply <- function(X, patterns,
                         replacements = as.list(paste(seq_along(patterns)))) {
  # Check the inputs
  if (class(patterns) != "character") {
    stop("'patterns' must be a character vector")
  }
  if (class(replacements) != "list") {
    stop("'replacements' must be a list")
  }
  if (!all(vapply(replacements, function(x) class(x) == "character", logical(1)))) {
    stop("'replacements' can only contain character vectors")
  }
  if (length(patterns) != length(replacements)) {
    stop("'patterns' and 'replacements' must have the same length")
  }

  # Keep track of which records have already been matched to a pattern.
  matchIndex <- rep(NA_integer_, length(X))

  # This will contain a replacement vector for each element in X.
  replacementList <- vector("list", length(replacements))

  for (i in seq_along(patterns)) {
    # Match the pattern to the record, omitting items already matched.
    matchData <- regexec(patterns[i], X[is.na(matchIndex)])

    # Find all the places where matches occurred.
    matches <- vapply(matchData, `[`, integer(1), 1) != -1
    numMatches <- sum(matches)

    # Where are new matches?
    matchedHere <- rep(FALSE, length(X))
    matchedHere[is.na(matchIndex)] <- matches

    # Figure out which of the replacements are backreferences (and not just
    # strings).
    indexReplacements <- vapply(regmatches(replacements[[i]],
                                           regexec("^\\\\([1-9])$",
                                                   replacements[[i]])),
                                function(x) as.integer(x[2]),
                                integer(1))
    stringReplacements <- replacements[[i]]
    stringReplacements[!is.na(indexReplacements)] <- NA

    # Grab the matched groups
    matchedStrings <- regmatches(X[matchedHere], matchData[matches])

    # Create a replacement matrix. Each column is the replacement vector for a
    # record.
    backreferenceMatrix <- vapply(matchedStrings, `[`,
                                  character(length(indexReplacements)),
                                  indexReplacements+1)
    replacementMatrix <- matrix(rep(stringReplacements, numMatches),
                                ncol = numMatches)
    replacementMatrix[is.na(replacementMatrix)] <- backreferenceMatrix[is.na(replacementMatrix)]

    # Fill in the data for the new matches.
    replacementList[matchedHere] <- lapply(seq_len(numMatches),
                                           function(i) replacementMatrix[, i])

    # Update the indices of the matches
    matchIndex[matchedHere] <- i
  }

  # Using col_names to hopefully support named vectors in the future.
  maxReplacements <- max(vapply(replacements, length, integer(1)))
  attr(replacementList, "col_names") <- paste0("match_", seq_len(maxReplacements))

  # This will probably be useful for people.
  attr(replacementList, "match_index") <- matchIndex

  # S3 Object
  class(replacementList) <- "replacement_list"

  return(replacementList)
}
