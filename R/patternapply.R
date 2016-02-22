
#' Iteratively try patterns against a character vector.
#' 
#' @param X A character vector where matches are sought.
#' @param patterns A vector of regular expression patterns.
#' @param replacements A list of replacement information, must match the length
#'   of \code(patterns). Each element must be a character vector. This can
#'   include backreferences "\1" to "\9" to parenthesized subexpressions of the
#'   corresponding pattern.
#'   
#' @return A list of replacement vectors with class "replacement_list".
patternapply <- function(X, patterns,
                         replacements = list(paste(seq_along(patterns)))) {
  # Keep track of which records have already been matched to a pattern.
  matchIndex <- rep(NA_integer_, length(X))

  # This will contain a replacement vector for each element in X.
  replacementList <- list()
  
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
  
  return(replacementList)
}
