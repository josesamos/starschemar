
#' Transform a `tibble` to join
#'
#' Transform all fields in a `tibble` to character type and replace the `NA`
#' with a specific value.
#'
#' @param tb A `tibble`.
#'
#' @return A `tibble`.
#' @keywords internal
prepare_join <- function(tb) {
  geomultistar:::prepare_join(tb)
}


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
    geomultistar:::new_fact_table(ft, name, measures, agg_functions, nrow_agg)
  }


#' Reference a dimension
#'
#' Given a dimension, transform the fact table so that the attributes of the
#' dimension indicated as a parameter, which are in the fact table, are replaced
#' by the other attributes of the dimension.
#'
#' It is used to replace a set of attributes in the fact table with the
#' generated key of the dimension.
#'
#' If necessary, it is also used for the inverse operation: replace the
#' generated key with the rest of attributes (dereference a dimension).
#'
#' @param ft A `fact_table` object.
#' @param dimension A `dimension_table` object.
#' @param attributes A vector of attribute names, attributes used to reference the dimension.
#' @param conversion A boolean, indicates whether the attributes need to be
#'   transformed.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
reference_dimension <-
  function(ft, dimension, attributes, conversion = TRUE) {
    geomultistar:::reference_dimension(ft, dimension, attributes, conversion)
   }



#' Dereference a dimension
#'
#' Given a dimension, transform the fact table so that the primary key of the
#' dimension (which is a foreign key in the fact table) is replaced by the other
#' attributes of the dimension.
#'
#' @param ft A `fact_table` object.
#' @param dimension A `dimension_table` object.
#' @param conversion A boolean, indicates whether the attributes need to be
#'   transformed.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
dereference_dimension <-
  function(ft, dimension, conversion = TRUE) {
    geomultistar:::dereference_dimension(ft, dimension, conversion)
  }




#' Group the records in the table
#'
#' Group the records in the table using the aggregation functions for the
#' measurements.
#'
#' @param ft A `fact_table` object.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
group_table <- function(ft) {
  geomultistar:::group_table(ft)
}

