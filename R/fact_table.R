
# set_fact_name ------------------------------------------------------

#' Set fact name
#'
#' It allows us to define the name of facts.
#'
#' Attributes can be accessed directly but this function has been defined
#' because it is used from other classes and is thus done in a more controlled
#' way.
#'
#' @param ft A `fact_table` object.
#' @param name A string, name of fact.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
set_fact_name <- function(ft, name) {
  UseMethod("set_fact_name")
}


#' @rdname set_fact_name
#' @export
#' @keywords internal
set_fact_name.fact_table <- function(ft, name) {
  attr(ft, "name") <- name
  ft
}

