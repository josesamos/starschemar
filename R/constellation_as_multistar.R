#' Export a constellation as a multistar
#'
#' Once we have refined the format or content of facts and dimensions, we can
#' obtain a `multistar`. A `multistar` only distinguishes between general and
#' conformed dimensions, each dimension has its own data. It can contain
#' multiple fact tables.
#'
#' @param ct A `constellation` object.
#'
#' @return A `multistar` object.
#'
#' @family results export functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' ms <- ct_mrs %>%
#'   constellation_as_multistar()
#'
#' @export
constellation_as_multistar <- function(ct) {
  UseMethod("constellation_as_multistar")
}


#' @rdname constellation_as_multistar
#' @export
constellation_as_multistar.constellation <- function(ct) {
  dl <- list()
  names <- c()
  for (d in seq_along(ct$dimension)) {
    dl <- c(dl, list(ct$dimension[[d]]))
    names <- c(names, attr(ct$dimension[[d]], "name"))
  }
  names(dl) <- names
  fl <- list()
  for (s in seq_along(ct$star)) {
    st <- star_schema_as_mst(ct$star[[s]], fl, dl, names)
    fl <- st$fact
    dl <- st$dimension
  }
  st
}
