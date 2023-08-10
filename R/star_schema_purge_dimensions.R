
#' Purge dimensions
#'
#' Delete instances of dimensions not related to facts in a star schema.
#'
#' @param st A `star_schema` object.
#'
#' @return A `star_schema` object.
#'
#' @family incremental refresh functions
#'
#' @examples
#' library(tidyr)
#'
#' st <- st_mrs_age %>%
#'   purge_dimensions_star_schema()
#'
#' @export
purge_dimensions_star_schema <- function(st) {
  UseMethod("purge_dimensions_star_schema")
}


#' @rdname purge_dimensions_star_schema
#' @export
purge_dimensions_star_schema.star_schema <- function(st) {
  key <- NULL
  dimension_names <- get_dimension_names(st)
  for (name in dimension_names) {
    dimension <- get_dimension(st, name)
    k <- generics::setdiff(dimension[[1]], unique(st$fact[[1]][[sprintf("%s_key", name)]]))
    if (is.null(key)) {
      key <- list(name = k)
      names(key) <- name
    } else {
      dim_names <- names(key)
      key <- c(key, list(name = k))
      names(key) <- c(dim_names, name)
    }
  }
  for (name in names(st$dimension)) {
    if (!is_role_dimension(st$dimension[[name]])) {
      if (is_role_playing_dimension(st$dimension[[name]])) {
        k = NULL
        for (n in get_role_dimension_names(st, name)) {
          if (is.null(k)) {
            k <- key[[n]]
          } else {
            k <- intersect(k, key[[n]])
          }
        }
      } else {
        k <-  key[[name]]
      }
      if (length(k) > 0) {
        st$dimension[[name]] <- st$dimension[[name]][!(st$dimension[[name]][[1]] %in% k), ]
      }
    }
  }
  st
}


#' Get role dimension names associated to a role-playing dimension
#'
#' Each role dimension has the name of the role-playing dimension associated.
#' This function allows us to obtain role dimension names for a role-playing
#' dimension.
#'
#' @param dimension A `star_schema` object.
#'
#' @return A vector of dimension names.
#'
#' @keywords internal
get_role_dimension_names <- function(st, name) {
  role_dimensions <- NULL
  for (n in names(st$dimension)) {
    if (get_role_playing_dimension_name(st$dimension[[n]]) == name) {
      role_dimensions <- c(role_dimensions, n)
    }
  }
  role_dimensions
}


