

#' Title
#'
#' @param ct A `constellation` object.
#'
#' @return A `constellation` object.
#'
#' @examples
#'
#' @keywords internal
conform_all_dimensions <- function(ct) {
  UseMethod("conform_all_dimensions")
}


#' @rdname conform_all_dimensions
#' @export
#' @keywords internal
conform_all_dimensions.constellation <- function(ct) {
  names <- c()
  for (s in seq_along(ct$star)) {
    names <- c(names, get_dimension_names(ct$star[[s]]))
  }
  dim_names <- unique(names)
  for (dim in dim_names) {
    if (sum(names == dim) > 1) {
      ct <- conform_dimensions(ct, dim_name = dim)
    }
  }
  ct
}
