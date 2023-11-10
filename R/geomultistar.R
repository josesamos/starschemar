
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


#' Export a `multistar` as a flat table
#'
#' We can obtain a flat table, implemented using a `tibble`, from a `multistar`
#' (which can be the result of a query). If it only has one fact table, it is
#' not necessary to provide its name.
#'
#' @param ms A `multistar` object.
#' @param fact A string, name of the fact.
#'
#' @return A `tibble`.
#'
#' @family results export functions
#'
#' @examples
#'
#' ft <- ms_mrs |>
#'   multistar_as_flat_table(fact = "mrs_age")
#'
#' ms <- dimensional_query(ms_mrs) |>
#'   select_dimension(name = "where",
#'                    attributes = c("city", "state")) |>
#'   select_dimension(name = "when",
#'                    attributes = c("when_happened_year")) |>
#'   select_fact(name = "mrs_age",
#'               measures = c("n_deaths")) |>
#'   select_fact(
#'     name = "mrs_cause",
#'     measures = c("pneumonia_and_influenza_deaths", "other_deaths")
#'   ) |>
#'   filter_dimension(name = "when", when_happened_week <= "03") |>
#'   filter_dimension(name = "where", city == "Boston") |>
#'   run_query()
#'
#' ft <- ms |>
#'   multistar_as_flat_table()
#'
#' @export
multistar_as_flat_table <- function(ms, fact = NULL) {
  geomultistar:::multistar_as_flat_table(ms, fact)
}


#' Run query
#'
#' Once we have selected the facts, dimensions and defined the conditions on the
#' instances, we can execute the query to obtain the result.
#'
#' As an option, we can indicate if we do not want to unify the facts in the
#' case of having the same grain.
#'
#' @param dq A `dimensional_query` object.
#' @param unify_by_grain A boolean, unify facts with the same grain.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' ms <- dimensional_query(ms_mrs) |>
#'   select_dimension(name = "where",
#'                    attributes = c("city", "state")) |>
#'   select_dimension(name = "when",
#'                    attributes = c("when_happened_year")) |>
#'   select_fact(
#'     name = "mrs_age",
#'     measures = c("n_deaths"),
#'     agg_functions = c("MAX")
#'   ) |>
#'   select_fact(
#'     name = "mrs_cause",
#'     measures = c("pneumonia_and_influenza_deaths", "other_deaths")
#'   ) |>
#'   filter_dimension(name = "when", when_happened_week <= "03") |>
#'   filter_dimension(name = "where", city == "Boston") |>
#'   run_query()
#'
#' @export
run_query <- function(dq, unify_by_grain = TRUE) {
  geomultistar:::run_query(dq, unify_by_grain)
}

#' Filter dimension
#'
#' Allows you to define selection conditions for dimension rows.
#'
#' Conditions can be defined on any attribute of the dimension (not only on
#' attributes selected in the query for the dimension). The selection is made
#' based on the function `dplyr::filter`. Conditions are defined in exactly the
#' same way as in that function.
#'
#' @param dq A `dimensional_query` object.
#' @param name A string, name of the dimension.
#' @param ... Conditions, defined in exactly the same way as in `dplyr::filter`.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   filter_dimension(name = "when", when_happened_week <= "03") |>
#'   filter_dimension(name = "where", city == "Boston")
#'
#' @export
filter_dimension <- function(dq,
                             name = NULL,
                             ...) {
  geomultistar:::filter_dimension(dq, name, ...)
}


#' Select fact
#'
#' To define the fact to be consulted, its name is indicated, optionally, a
#' vector of names of selected measures and another of aggregation functions are
#' also indicated.
#'
#' If the name of any measure is not indicated, only the one corresponding to
#' the number of aggregated rows is included, which is always included.
#'
#' If no aggregation function is included, those defined for the measures are
#' considered.
#'
#' @param dq A `dimensional_query` object.
#' @param name A string, name of the fact.
#' @param measures A vector of measure names.
#' @param agg_functions A vector of aggregation function names. If none is
#'   indicated, those defined in the fact table are considered.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   select_fact(
#'     name = "mrs_age",
#'     measures = c("n_deaths"),
#'     agg_functions = c("MAX")
#'   )
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   select_fact(name = "mrs_age",
#'              measures = c("n_deaths"))
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   select_fact(name = "mrs_age")
#'
#' @export
select_fact <- function(dq,
                        name = NULL,
                        measures = NULL,
                        agg_functions = NULL) {
  geomultistar:::select_fact(dq, name, measures, agg_functions)
}


#' Select dimension
#'
#' To add a dimension in a `dimensional_query` object, we have to define its
#' name and a subset of the dimension attributes. If only the name of the
#' dimension is indicated, it is considered that all its attributes should be
#' added.
#'
#' @param dq A `dimensional_query` object.
#' @param name A string, name of the dimension.
#' @param attributes A vector of attribute names.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   select_dimension(name = "where",
#'                   attributes = c("city", "state")) |>
#'   select_dimension(name = "when")
#'
#' @export
select_dimension <- function(dq,
                             name = NULL,
                             attributes = NULL) {
  geomultistar:::select_dimension(dq, name, attributes)
}

#' `dimensional_query` S3 class
#'
#' An empty `dimensional_query` object is created where you can select fact
#' measures, dimension attributes and filter dimension rows.
#'
#' @param ms A `multistar` object.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' # ms_mrs <- ct_mrs |>
#' #  constellation_as_multistar()
#'
#' # dq <- dimensional_query(ms_mrs)
#'
#' @export
dimensional_query <- function(ms = NULL) {
  geomultistar:::dimensional_query(ms)
}
