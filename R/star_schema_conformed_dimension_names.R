
#' Title
#'
#' @param st
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
conformed_dimension_names <- function(st) {
  UseMethod("conformed_dimension_names")
}


#' @rdname conformed_dimension_names
#' @export
#' @keywords internal
conformed_dimension_names.star_schema <- function(st) {
  res <- c()
  names <- names(st$dimension)
  for (n in names) {
    if(is_conformed_dimension(st$dimension[[n]])) {
      res <- c(res, n)
    }
  }
  res
}

