
#' Dereference a dimension
#'
#' Given a dimension, transform the fact table so that the primary key of the
#' dimension (which is a foreign key in the fact table) is replaced by the other
#' attributes of the dimension.
#'
#' @param ft A `fact_table` object.
#' @param dimension A `dimension_table` object.
#' @param conversion A boolean, indicates whether the attributes need to be
#'   transformed.
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
