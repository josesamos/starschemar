
# get_conformed_dimension_names_st ----------------------------------------

#' Get conformed dimension names
#'
#' Get the names of the star schema conformed dimensions.
#'
#' @param st A `star_schema` object.
#'
#' @return A vector of dimension names.
#'
#' @keywords internal
get_conformed_dimension_names_st <- function(st) {
  UseMethod("get_conformed_dimension_names_st")
}


#' @rdname get_conformed_dimension_names_st
#' @export
#' @keywords internal
get_conformed_dimension_names_st.star_schema <- function(st) {
  res <- c()
  names <- names(st$dimension)
  for (n in names) {
    if(is_conformed_dimension(st$dimension[[n]])) {
      res <- c(res, n)
    }
  }
  res
}


# get_fact_name -----------------------------------------------------------

#' Get fact name
#'
#' Get the name of the fact table.
#'
#' @param st A `star_schema` object.
#'
#' @return A string, name of the fact table.
#'
#' @keywords internal
get_fact_name <- function(st) {
  UseMethod("get_fact_name")
}


#' @rdname get_fact_name
#' @export
#' @keywords internal
get_fact_name.star_schema <- function(st) {
  attr(st$fact, "name")
}

