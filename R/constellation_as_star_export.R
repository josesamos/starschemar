#' Title
#'
#' @param ct A `constellation` object.
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
constellation_as_star_export <- function(ct) {
  UseMethod("constellation_as_star_export")
}


#' @rdname constellation_as_star_export
#' @export
constellation_as_star_export.constellation <- function(ct) {
  dl <- list()
  names <- c()
  for (d in seq_along(ct$dimension)) {
    dl <- c(dl, list(ct$dimension[[d]]))
    names <- c(names, attr(ct$dimension[[d]], "name"))
  }
  names(dl) <- names
  fl <- list()
  for (s in seq_along(ct$star)) {
    st <- star_schema_as_star_export(ct$star[[s]], fl, dl, names)
    fl <- st$fact
    dl <- st$dimension
  }
  st
}
