#' Title
#'
#' @param ct A `constellation` object.
#' @param include_role_playing
#'
#' @return A `star_export` object.
#'
#' @family constellation functions
#' @seealso
#'
#' @examples
#'
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
        star_schema_as_tibble_list(ct$star[[s]], tl, names, include_role_playing)
    }
    tl
  }
