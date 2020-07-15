
#' Title
#'
#' @param ft A `fact_table` object.
#' @param dimension A `dimension_table` object.
#' @param conversion
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
dereference_dimension <-
  function(ft, dimension, conversion = TRUE) {
    UseMethod("dereference_dimension")
  }

#' @rdname dereference_dimension
#' @export
#' @keywords internal
dereference_dimension.fact_table <-
  function(ft, dimension, conversion = TRUE) {
    reference_dimension(ft, dimension, names(dimension)[1], conversion)
  }
