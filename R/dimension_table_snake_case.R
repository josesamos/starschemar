

#' Title
#'
#' @param dim
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
snake_case_dimension <- function(dim) {
  UseMethod("snake_case_dimension")
}


#' @rdname snake_case_dimension
#' @export
#' @keywords internal
snake_case_dimension.dimension_table <- function(dim) {
  sep = "_"
  attr(dim, "name") <-
    snakecase::to_snake_case(attr(dim, "name"), sep_out = sep)
  names(dim) <-
    snakecase::to_snake_case(names(dim), sep_out = sep)
  if (is_role_dimension(dim)) {
    attr(dim, "role_playing") <-
      snakecase::to_snake_case(attr(dim, "role_playing"), sep_out = sep)
  }
  dim
}
