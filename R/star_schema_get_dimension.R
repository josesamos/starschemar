
# get_dimension -----------------------------------------------------------

#' Get dimension
#'
#' Get a dimension of a star schema given its name.
#'
#' Role dimensions can be obtained but not role playing dimensions. Role
#' dimensions get their instances of role playing dimensions.
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#'
#' @return A `dimension_table` object.
#'
#' @family data cleaning functions
#'
#' @examples
#' library(tidyr)
#'
#' d <- st_mrs_age %>%
#'   get_dimension("when")
#'
#' @export
get_dimension <- function(st, name) {
  UseMethod("get_dimension")
}


#' @rdname get_dimension
#' @export
get_dimension.star_schema <- function(st, name) {
  dim <- NULL
  if (name %in% names(st$dimension)) {
    dim <- st$dimension[[name]]
    if (is_role_dimension(dim)) {
      type <- get_dimension_type(dim)
      rp_name <- get_role_playing_dimension_name(dim)
      shared_dim <- st$dimension[[rp_name]]
      names(shared_dim) <- names(dim)  # role dim. only stores names
      dim <- set_dimension_name(shared_dim, name)
      dim <- set_dimension_type(dim, type)
      dim <- set_role_playing_dimension_name(dim, rp_name)
    } else if (is_role_playing_dimension(dim)) {
      dim <- NULL
    }
  }
  dim
}



# get_dimension_names -----------------------------------------------------

#' Get dimension names
#'
#' Get the names of the dimensions of a star schema.
#'
#' Role playing dimensions are not considered.
#'
#' @param st A `star_schema` object.
#'
#' @return A vector of dimension names.
#'
#' @family data cleaning functions
#'
#' @examples
#' library(tidyr)
#'
#' dn <- st_mrs_age %>%
#'   get_dimension_names()
#'
#' @export
get_dimension_names <- function(st) {
  UseMethod("get_dimension_names")
}


#' @rdname get_dimension_names
#' @export
get_dimension_names.star_schema <- function(st) {
  res <- c()
  names <- names(st$dimension)
  for (n in names) {
    if (!is_role_playing_dimension(st$dimension[[n]])) {
      res <- c(res, n)
    }
  }
  res
}
