
#' Transform a dimension into a role dimension
#'
#' Once the role-playing dimension has been generated, the dimensions from which
#' it has been defined are transformed into role dimensions. Records are removed
#' as they are obtained from the role-playing dimension.
#'
#' @param dimension A `dimension_table` object.
#' @param role_playing_name A string, name of role-playing dimension.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
role_dimension <- function(dimension, role_playing_name) {
  UseMethod("role_dimension")
}


#' @rdname role_dimension
#' @export
#' @keywords internal
role_dimension.dimension_table <- function(dimension, role_playing_name) {
  name <- attr(dimension, "name")
  class <- attr(dimension, "class")
  dimension <-
    tibble::as_tibble(stats::setNames(data.frame(matrix(
      ncol = length(names(dimension)), nrow = 0
    )), names(dimension)))
  attr(dimension, "name") <- name
  attr(dimension, "class") <- class
  attr(dimension, "type") <- "role"
  attr(dimension, "role_playing") <- role_playing_name
  dimension
}
