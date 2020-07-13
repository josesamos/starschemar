

#' Title
#'
#' @param st A `star_schema` object.
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
star_schema_as_flat_table <- function(st) {
  UseMethod("star_schema_as_flat_table")
}


#' @rdname star_schema_as_flat_table
#' @export
star_schema_as_flat_table.star_schema <- function(st) {
  dim <- get_all_dimensions(st)
  for (d in seq_along(dim)) {
    st$fact[[1]] <-
      dereference_dimension(st$fact[[1]], dim[[d]], conversion = FALSE)
  }
  tibble::as_tibble(st$fact[[1]])
}
