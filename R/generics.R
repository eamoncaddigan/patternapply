
#' Convert a replacement_list object into a character matrix.
#'
#' @param x A replacement_list.
#'
#' @return A character matrix. Each row corresponds to an entry in the
#'   replacement_list.
#'
#' @method as.matrix replacement_list
as.matrix.replacement_list <- function(x) {
  rlMatrix <- matrix(NA_character_,
                     nrow = length(x),
                     ncol = length(attr(x, "col_names")))
  colnames(rlMatrix) <- attr(x, "col_names")

  # Loop (I know) through the elements in the replacement list and copy them
  # over to the new matrix.
  # XXX: This will need to be changed if named vectors are recognized by
  # patternapply().
  for (i in seq_along(x)) {
    rlMatrix[i, seq_along(x[[i]])] <- x[[i]]
  }

  return(rlMatrix)
}

#' Convert a replacement_list object into a data.frame.
#'
#' @param x A replacement_list.
#'
#' @return A data.frame. Each row corresponds to an entry in the
#'   replacement_list.
#'
#' @method as.data.frame replacement_list
as.data.frame.replacement_list <- function(x) {
  return(as.data.frame.matrix(as.matrix.replacement_list(x)))
}
