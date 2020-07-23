

# get_conformed_dimension -------------------------------------------------

#' Get conformed dimension
#'
#' Get a conformed dimension of a constellation given its name.
#'
#' @param ct A `constellation` object.
#' @param name A string, name of the dimension.
#'
#' @return A `dimension_table` object.
#'
#' @family constellation functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' d <- ct_mrs %>%
#'   get_conformed_dimension("when")
#'
#' @export
get_conformed_dimension <- function(ct, name) {
  UseMethod("get_conformed_dimension")
}


#' @rdname get_conformed_dimension
#' @export
get_conformed_dimension.constellation <- function(ct, name) {
  dim <- NULL
  if (name %in% names(ct$dimension)) {
    dim <- ct$dimension[[name]]
  }
  dim
}



# get_conformed_dimension_names -------------------------------------------

#' Get conformed dimension names
#'
#' Get the names of the conformed dimensions of a constellation.
#'
#' @param ct A `constellation` object.
#'
#' @return A vector of dimension names.
#'
#' @family constellation functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' d <- ct_mrs %>%
#'   get_conformed_dimension_names()
#'
#' @export
get_conformed_dimension_names <- function(ct) {
  UseMethod("get_conformed_dimension_names")
}


#' @rdname get_conformed_dimension_names
#' @export
get_conformed_dimension_names.constellation <- function(ct) {
  names(ct$dimension)
}

