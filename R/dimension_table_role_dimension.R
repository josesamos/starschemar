
#' Title
#'
#' @param dim
#' @param d_new_name
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
role_dimension <- function(dim, d_new_name) {
  UseMethod("role_dimension")
}


#' @rdname role_dimension
#' @export
#' @keywords internal
role_dimension.dimension_table <- function(dim, d_new_name) {
  name <- attr(dim, "name")
  class <- attr(dim, "class")
  dim <-
    tibble::as_tibble(setNames(data.frame(matrix(
      ncol = length(names(dim)), nrow = 0
    )), names(dim)))
  attr(dim, "name") <- name
  attr(dim, "class") <- class
  attr(dim, "type") <- "role"
  attr(dim, "role_playing") <- d_new_name
  dim
}
