
#' Define facts in a `dimensional_model` object
#'
#' To define facts in a `dimensional_model` object, the essential data is a name
#' and a set of measurements that can be empty (does not have explicit
#' measurements). Associated with each measurement, an aggregation function is
#' required, which by default is SUM.
#'
#' To get a star schema (a `star_schema` object) we need a flat table
#' (implemented through a `tibble`) and a `dimensional_model` object. The
#' definition of facts in the `dimensional_model` object is made from the flat
#' table column names. Using the `dput` function we can list the column names of
#' the flat table so that we do not have to type their names.
#'
#' Associated with each measurement there is an aggregation function that can be
#' SUM, MAX or MIN. Mean is not considered among the possible aggregation
#' functions: The reason is that calculating the mean by considering subsets of
#' data does not necessarily yield the mean of the total data.
#'
#' An additional measurement corresponding to the number of aggregated rows is
#' always added which, together with SUM, allows us to obtain the mean if
#' needed.
#'
#' @param st A `dimensional_model` object.
#' @param name A string, name of the fact.
#' @param measures A vector of measure names.
#' @param agg_functions A vector of aggregation function names. If none is
#'   indicated, the default is SUM. Additionally they can be MAX or MIN.
#' @param nrow_agg A string, measurement name for the number of rows aggregated.
#'
#' @return A `dimensional_model` object.
#'
#' @family star definition functions
#'
#' @examples
#'
#' # dput(colnames(mrs_age))
#' #
#' # c(
#' #   "Reception Year",
#' #   "Reception Week",
#' #   "Reception Date",
#' #   "Data Availability Year",
#' #   "Data Availability Week",
#' #   "Data Availability Date",
#' #   "Year",
#' #   "WEEK",
#' #   "Week Ending Date",
#' #   "REGION",
#' #   "State",
#' #   "City",
#' #   "Age Range",
#' #   "Deaths"
#' # )
#'
#' dm <- dimensional_model() |>
#'   define_fact(
#'     name = "mrs_age",
#'     measures = c("Deaths"),
#'     agg_functions = c("SUM"),
#'     nrow_agg = "nrow_agg"
#'   )
#'
#' dm <- dimensional_model() |>
#'   define_fact(
#'     name = "mrs_age",
#'     measures = c("Deaths")
#'   )
#'
#' dm <- dimensional_model() |>
#'   define_fact(name = "Factless fact")
#'
#' @export
define_fact <- function(st,
                        name = NULL,
                        measures = NULL,
                        agg_functions = NULL,
                        nrow_agg = "nrow_agg") {
  UseMethod("define_fact")
}


#' @rdname define_fact
#' @export
define_fact.dimensional_model <- function(st,
                                        name = NULL,
                                        measures = NULL,
                                        agg_functions = NULL,
                                        nrow_agg = "nrow_agg") {
  stopifnot("The name of facts must be indicated." = !is.null(name))
  if (is.null(agg_functions)) {
    agg_functions <-  rep("SUM", length(measures))
  }
  stopifnot("Measures and aggregation functions do not correspond." = length(measures) == length(agg_functions))
  for (af in agg_functions) {
    validate_names(c("SUM", "MAX", "MIN"), af, concept = 'aggregation function')
  }
  stopifnot("There are repeated measures." = length(c(measures, nrow_agg)) == length(unique(c(measures, nrow_agg))))
  attributes_defined <- get_attribute_names(st)
  for (measure in c(measures, nrow_agg)) {
    stopifnot("There are measures that have already been defined." = !(measure %in% attributes_defined))
  }
  st$fact <-
    list(
      name = name,
      measures = measures,
      agg_functions = agg_functions,
      nrow_agg = nrow_agg
    )
  st
}
