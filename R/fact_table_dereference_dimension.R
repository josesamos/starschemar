
#' Title
#'
#' @param ft
#' @param dim
#' @param conversion
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
dereference_dimension <-
  function(ft, dim, conversion = TRUE) {
    UseMethod("dereference_dimension")
  }

#' @rdname dereference_dimension
#' @export
#' @keywords internal
dereference_dimension.fact_table <-
  function(ft, dim, conversion = TRUE) {
    reference_dimension(ft, dim, names(dim)[1], conversion)
  }
