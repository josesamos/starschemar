
#' Title
#'
#' @param st
#'
#' @return
#'
#' @family star schema functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' @export
get_dimension_names <- function(st) {
  UseMethod("get_dimension_names")
}


#' @rdname get_dimension_names
#' @export
get_dimension_names.star_schema <- function(st) {
  res <- c()
  names <- names(st$dimension)
  for (n in names) {
    if (!is_role_playing_dimension(st$dimension[[n]])) {
      res <- c(res, n)
    }
  }
  res
}
