
#' Title
#'
#' @param st
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
fact_name <- function(st) {
  UseMethod("fact_name")
}


#' @rdname fact_name
#' @export
#' @keywords internal
fact_name.star_schema <- function(st) {
  attr(st$fact, "name")
}
