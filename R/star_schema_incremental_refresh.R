

#' Title
#'
#' existing = "ignore"/"replace"/"group"
#'
#' @param st
#' @param st_new
#' @param existing
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
incremental_refresh <- function(st, st_new, existing = "ignore") {
  UseMethod("incremental_refresh")
}


#' @rdname incremental_refresh
#' @export
#' @keywords internal
incremental_refresh.star_schema <-
  function(st, st_new, existing = "ignore") {
    stopifnot(existing %in% c("ignore", "replace", "group"))

    name_dims <- get_name_of_uniquely_implemented_dimensions(st_new)
    for (n in name_dims) {
      dim <-
        incremental_refresh_dimension(get_dimension(st, n), get_dimension(st_new, n))
      st_new <-
        conform_dimension(st_new, n, dim, incremental_update = TRUE)
      st <- replace_dimension(st, n, dim)
    }
    st$fact[[1]] <-
      incremental_refresh_fact(st$fact[[1]], st_new$fact[[1]], existing)
    st
  }





#' Title
#'
#' @param st
#'
#' @return
#' @keywords internal
#' @noRd
#'
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

