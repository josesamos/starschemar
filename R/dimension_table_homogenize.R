
#' Homogenize a dimension
#'
#' To merge dimensions, they must first be homogenized: the generated primary
#' key must be removed and, if necessary, its attributes (columns) must be
#' renamed.
#'
#' @param dimension A `dimension_table` object.
#' @param attributes A vector of attribute names of the dimension.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
homogenize <- function(dimension, attributes = NULL) {
  UseMethod("homogenize")
}


#' @rdname homogenize
#' @export
#' @keywords internal
homogenize.dimension_table <- function(dimension, attributes = NULL) {
  dimension <- dimension[,-1]
  if (!is.null(attributes)) {
    names(dimension) <- attributes
  }
  dimension
}
