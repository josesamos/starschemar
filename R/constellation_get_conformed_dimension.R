

#' Title
#'
#' @param ct A `constellation` object.
#' @param dim_name
#'
#' @return A `star_export` object.
#'
#' @family constellation functions
#' @seealso
#'
#' @examples
#'
#'
#' @export
get_conformed_dimension <- function(ct, dim_name) {
  UseMethod("get_conformed_dimension")
}


#' @rdname get_conformed_dimension
#' @export
get_conformed_dimension.constellation <- function(ct, dim_name) {
  dim <- NULL
  if (dim_name %in% names(ct$dimension)) {
    dim <- ct$dimension[[dim_name]]
  }
  dim
}
