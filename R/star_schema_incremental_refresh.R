
#' Incrementally refresh a star schema with another
#'
#' Incrementally refresh a star schema with the content of a new one that is
#' integrated into the first.
#'
#' Once the dimensions are integrated, if there are records in the fact table
#' whose keys match the new ones, new ones can be ignored, they can be replaced
#' by new ones, all of them can be grouped using the aggregation functions, or
#' they can be deleted. Therefore, the possible values of the `existing`
#' parameter are: "ignore", "replace", "group" or "delete".
#'
#' @param st A `star_schema` object.
#' @param st_new A `star_schema` object.
#' @param existing A string, operation to be performed with records in the fact
#'   table whose keys match.
#'
#' @examples
#' library(tidyr)
#'
#' st <- st_mrs_age %>%
#'   incremental_refresh(st_mrs_age_w10, existing = "replace")
#'
#' st <- st_mrs_cause %>%
#'   incremental_refresh(st_mrs_cause_w10, existing = "group")
#'
#' @export
incremental_refresh <- function(st, st_new, existing = "ignore") {
  UseMethod("incremental_refresh")
}


#' @rdname incremental_refresh
#' @export
#' @keywords internal
incremental_refresh.star_schema <-
  function(st, st_new, existing = "ignore") {
    stopifnot(existing %in% c("ignore", "replace", "group", "delete"))

    dimensions <-
      get_name_of_uniquely_implemented_dimensions(st_new)
    for (d in dimensions) {
      dim <-
        incremental_refresh_dimension(get_dimension(st, d), get_dimension(st_new, d))
      st_new <- replace_dimension_in_facts(st_new, d, dim)
      st <- replace_dimension(st, d, dim)
    }
    st$fact[[1]] <-
      incremental_refresh_fact(st$fact[[1]], st_new$fact[[1]], existing)
    st
  }



#' Get name of uniquely implemented dimensions
#'
#' Get a list of dimension names that are uniquely implemented.
#'
#' For role dimensions that share role playing dimension, only one is
#' considered. Role playing dimensions are not considered.
#'
#' @param st A `star_schema` object.
#'
#' @return A vector of dimension names.
#' @keywords internal
get_name_of_uniquely_implemented_dimensions <- function(st) {
  res <- c()
  names <- names(st$dimension)
  rpd_names <- c()
  for (n in names) {
    if (!is_role_playing_dimension(st$dimension[[n]])) {
      if (is_role_dimension(st$dimension[[n]])) {
        rpd_n <- get_role_playing_dimension_name(st$dimension[[n]])
        if (!(rpd_n %in% rpd_names)) {
          res <- c(res, n)
          rpd_names <- c(rpd_names, rpd_n)
        }
      } else {
        res <- c(res, n)
      }
    }
  }
  res
}
