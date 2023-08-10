#' `fact_table` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param ft A `tibble`, contains the fact table.
#' @param name A string, name of the fact.
#' @param measures A vector of measurement names.
#' @param agg_functions A vector of aggregation function names.
#' @param nrow_agg A string, measurement name for the number of rows aggregated.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
new_fact_table <-
  function(ft = tibble::tibble(),
           name = NULL,
           measures = NULL,
           agg_functions = NULL,
           nrow_agg = NULL) {
    # Check the type of the base object
    stopifnot(tibble::is_tibble(ft))
    stopifnot(!is.null(name))

    fk <- c()
    for (n in names(ft)) {
      if (!(n %in% measures)) {
        fk <- c(fk, n)
      }
    }

    structure(
      ft,
      class = unique(c("fact_table", class(ft))),
      name = name,
      foreign_keys = fk,
      measures = measures,
      agg_functions = agg_functions,
      nrow_agg = nrow_agg
    )
  }


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

