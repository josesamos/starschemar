

# get_star_schema -------------------------------------------------

#' Get star schema
#'
#' Get a star schema of a constellation given its name.
#'
#' @param ct A `constellation` object.
#' @param name A string, name of the star schema.
#'
#' @return A `dimension_table` object.
#'
#' @family incremental refresh functions
#'
#' @examples
#' library(tidyr)
#'
#' d <- ct_mrs %>%
#'   get_star_schema("mrs_age")
#'
#' @export
get_star_schema <- function(ct, name) {
  UseMethod("get_star_schema")
}


#' @rdname get_star_schema
#' @export
get_star_schema.constellation <- function(ct, name) {
  st <- NULL
  if (name %in% names(ct$star)) {
    st <- ct$star[[name]]
  }
  st
}



# get_star_schema_names -------------------------------------------

#' Get star schema names
#'
#' Get the names of the star schemas in a constellation.
#'
#' @param ct A `constellation` object.
#'
#' @return A vector of star schema names.
#'
#' @family incremental refresh functions
#'
#' @examples
#' library(tidyr)
#'
#' d <- ct_mrs %>%
#'   get_star_schema_names()
#'
#' @export
get_star_schema_names <- function(ct) {
  UseMethod("get_star_schema_names")
}


#' @rdname get_star_schema_names
#' @export
get_star_schema_names.constellation <- function(ct) {
  names(ct$star)
}

