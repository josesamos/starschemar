#' Export a constellation as a tibble list
#'
#' Once we have refined the format or content of facts and dimensions, we can
#' obtain a `tibble` list with them. Role playing dimensions can be optionally
#' included.
#'
#' @param ct A `constellation` object.
#' @param include_role_playing A boolean.
#'
#' @return A list of `tibble` objects.
#'
#' @family constellation export functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' tl <- ct_mrs %>%
#'   constellation_as_tibble_list()
#'
#' tl <- ct_mrs %>%
#'   constellation_as_tibble_list(include_role_playing = TRUE)
#'
#' @export
constellation_as_tibble_list <-
  function(ct, include_role_playing = FALSE) {
    UseMethod("constellation_as_tibble_list")
  }


#' @rdname constellation_as_tibble_list
#' @export
constellation_as_tibble_list.constellation <-
  function(ct, include_role_playing = FALSE) {
    tl <- list()
    names <- c()
    for (d in seq_along(ct$dimension)) {
      tl <- c(tl, list(tibble::as_tibble(ct$dimension[[d]])))
      names <- c(names, attr(ct$dimension[[d]], "name"))
    }
    names(tl) <- names
    for (s in seq_along(ct$star)) {
      tl <-
        star_schema_as_tl(ct$star[[s]], tl, names, include_role_playing)
    }
    tl
  }
