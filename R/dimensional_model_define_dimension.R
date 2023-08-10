
#' Define dimensions in a `dimensional_model` object
#'
#' To define a dimension in a `dimensional_model` object, we have to define its
#' name and the set of attributes that make it up.
#'
#' To get a star schema (a `star_schema` object) we need a flat table
#' (implemented through a `tibble`) and a `dimensional_model` object. The
#' definition of dimensions in the `dimensional_model` object is made from the
#' flat table column names. Using the `dput` function we can list the column
#' names of the flat table so that we do not have to type their names.
#'
#' @param st A `dimensional_model` object.
#' @param name A string, name of the dimension.
#' @param attributes A vector of attribute names.
#'
#' @return A `dimensional_model` object.
#'
#' @family star definition functions
#'
#' @examples
#' library(tidyr)
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
#' dm <- dimensional_model() %>%
#'   define_dimension(name = "When",
#'                    attributes = c("Week Ending Date",
#'                                   "WEEK",
#'                                   "Year")) %>%
#'   define_dimension(name = "When Available",
#'                    attributes = c("Data Availability Date",
#'                                   "Data Availability Week",
#'                                   "Data Availability Year")) %>%
#'   define_dimension(name = "Where",
#'                    attributes = c("REGION",
#'                                   "State",
#'                                   "City")) %>%
#'   define_dimension(name = "Who",
#'                    attributes = c("Age Range"))
#'
#' @export
define_dimension <- function(st,
                             name = NULL,
                             attributes = NULL) {
  UseMethod("define_dimension")
}



#' @rdname define_dimension
#' @export
define_dimension.dimensional_model <- function(st,
                             name = NULL,
                             attributes = NULL) {
  stopifnot(!is.null(name))
  stopifnot(!(name %in% names(st$dimension)))
  stopifnot(length(attributes) > 0)
  stopifnot(length(attributes) == length(unique(attributes)))
  attributes_defined <- get_attribute_names(st)
  for (attribute in attributes) {
    stopifnot(!(attribute %in% attributes_defined))
  }

  if (is.null(st$dimension)) {
    st$dimension <- list(name = attributes)
    names(st$dimension) <- name
  } else {
    dim_names <- names(st$dimension)
    st$dimension <- c(st$dimension, list(name = attributes))
    names(st$dimension) <- c(dim_names, name)
  }
  st
}
