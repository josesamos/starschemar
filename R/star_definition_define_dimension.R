
#' Define dimensions in a `star_definition` object
#'
#' To define a dimension in a `star_definition` object, we have to define its
#' name and the set of attributes that make it up.
#'
#' To get a star schema (a `star_schema` object) we need a flat table (implemented
#' through a `tibble`) and a `star_definition` object. The definition of dimensions in
#' the `star_definition` object is made from the flat table column names. Using
#' the `dput` function we can list the column names of the flat table so that we
#' do not have to type their names.
#'
#' @param st A `star_definition` object.
#' @param name A string, name of the dimension.
#' @param attributes A vector of attribute names.
#'
#' @return A `star_definition` object.
#'
#' @family star definition functions
#' @seealso
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
#' sd <- star_definition() %>%
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
define_dimension.star_definition <- function(st,
                             name = NULL,
                             attributes = NULL) {
  stopifnot(!is.null(name))
  stopifnot(length(attributes) > 0)
  stopifnot(!(name %in% names(st$dimension)))

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
