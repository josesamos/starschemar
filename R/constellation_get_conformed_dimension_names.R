

#' Title
#'
#' @param ct A `constellation` object.
#'
#' @return A list of names.
#'
#' @family constellation functions
#' @seealso
#'
#' @examples
#'
#'
#' @export
get_conformed_dimension_names <- function(ct) {
  UseMethod("get_conformed_dimension_names")
}


#' @rdname get_conformed_dimension_names
#' @export
get_conformed_dimension_names.constellation <- function(ct) {
  names(ct$dimension)
}
