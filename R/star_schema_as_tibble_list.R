
#' Export a star schema as a `tibble` list
#'
#' Once we have refined the format or content of facts and dimensions, we can
#' obtain a `tibble` list with them. Role playing dimensions can be optionally
#' included.
#'
#' @param st A `star_schema` object.
#' @param include_role_playing A boolean.
#'
#' @return A list of `tibble` objects.
#'
#' @family results export functions
#'
#' @examples
#'
#' tl <- st_mrs_age |>
#'   star_schema_as_tibble_list()
#'
#' tl <- st_mrs_age |>
#'   star_schema_as_tibble_list(include_role_playing = TRUE)
#'
#' @export
star_schema_as_tibble_list <-
  function(st,
           include_role_playing = FALSE) {
    UseMethod("star_schema_as_tibble_list")
  }


#' @rdname star_schema_as_tibble_list
#' @export
star_schema_as_tibble_list.star_schema <-
  function(st,
           include_role_playing = FALSE) {
    star_schema_as_tl(st, include_role_playing = include_role_playing)
  }


# Star schema as a tibble list (common) -----------------------------------

#' Export a star schema as a `tibble` list (common)
#'
#' @param st A `star_schema` object.
#' @param tl_prev A list of `tibble` objects.
#' @param commondim A list of dimension names already included.
#' @param include_role_playing A boolean.
#'
#' @return A `tibble` list.
#' @keywords internal
star_schema_as_tl <-
  function(st,
           tl_prev = NULL,
           commondim = NULL,
           include_role_playing) {
    UseMethod("star_schema_as_tl")
  }


#' @rdname star_schema_as_tl
#' @export
#' @keywords internal
star_schema_as_tl.star_schema <-
  function(st,
           tl_prev = NULL,
           commondim = NULL,
           include_role_playing) {
    names_prev <- names(tl_prev)
    tl <- c(tl_prev, list(tibble::as_tibble(st$fact[[1]])))
    names <- c(names_prev, attr(st$fact[[1]], "name"))
    dim <- get_all_dimensions(st)
    for (d in seq_along(dim)) {
      name_dim <- attr(dim[[d]], "name")
      if (!(name_dim %in% commondim)) {
        tl <- c(tl, list(tibble::as_tibble(dim[[d]])))
        names <- c(names, name_dim)
      }
    }
    if (include_role_playing) {
      rp_names <- get_name_of_role_playing_dimensions(st)
      for (d in rp_names) {
        if (!(d %in% names)) {
          tl <- c(tl, list(tibble::as_tibble(st$dimension[[d]])))
          names <- c(names, d)
        }
      }
    }
    names(tl) <- names
    tl
  }



#' Get the name of the role playing dimensions
#'
#' @param st A `star_schema` object.
#'
#' @return A vector of dimension names.
#' @keywords internal
get_name_of_role_playing_dimensions <- function(st) {
  res <- c()
  names <- names(st$dimension)
  for (n in names) {
    if (is_role_playing_dimension(st$dimension[[n]])) {
      res <- c(res, n)
    }
  }
  res
}


