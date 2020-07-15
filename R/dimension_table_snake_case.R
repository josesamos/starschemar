
#' Transform names according to the snake case style in a dimension
#'
#' Transform column, attribute and dimension names according to the snake case
#' style.
#'
#' @param dimension A `dimension_table` object.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
snake_case_dimension <- function(dimension) {
  UseMethod("snake_case_dimension")
}


#' @rdname snake_case_dimension
#' @export
#' @keywords internal
snake_case_dimension.dimension_table <- function(dimension) {
  sep = "_"
  attr(dimension, "name") <-
    snakecase::to_snake_case(attr(dimension, "name"), sep_out = sep)
  names(dimension) <-
    snakecase::to_snake_case(names(dimension), sep_out = sep)
  if (is_role_dimension(dimension)) {
    attr(dimension, "role_playing") <-
      snakecase::to_snake_case(attr(dimension, "role_playing"), sep_out = sep)
  }
  dimension
}
