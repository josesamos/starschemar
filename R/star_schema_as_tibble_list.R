
#' Title
#'
#' @param st A `star_schema` object.
#' @param tl_prev
#' @param commondim
#' @param include_role_playing
#'
#' @return
#'
#' @family star schema functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' @export
star_schema_as_tibble_list <-
  function(st,
           tl_prev = NULL,
           commondim = NULL,
           include_role_playing = FALSE) {
    UseMethod("star_schema_as_tibble_list")
  }


#' @rdname star_schema_as_tibble_list
#' @export
star_schema_as_tibble_list.star_schema <-
  function(st,
           tl_prev = NULL,
           commondim = NULL,
           include_role_playing = FALSE) {
    names_prev <- names(tl_prev)
    tl <- c(tl_prev, list(tibble::as_tibble(st$fact[[1]])))
    names <- attr(st$fact[[1]], "name")
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
        tl <- c(tl, list(tibble::as_tibble(st$dimension[[d]])))
        if (d %in% names) {
          names <- c(names, paste(attr(st$fact[[1]], "name"), d, sep = "_"))
        } else {
          names <- c(names, d)
        }
      }
    }
    names(tl) <- c(names_prev, names)
    tl
  }


#' Title
#'
#' @param st
#'
#' @return
#' @keywords internal
#' @noRd
#'
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


