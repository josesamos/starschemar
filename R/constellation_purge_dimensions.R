
#' Purge dimensions in a constellation
#'
#' Delete instances of dimensions not related to facts in a constellation.
#'
#' @param ct A `constellation` object.
#'
#' @return A `constellation` object.
#'
#' @family incremental refresh functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' ct <- ct_mrs %>%
#'   purge_dimensions_constellation()
#'
#' @export
purge_dimensions_constellation <- function(ct) {
  UseMethod("purge_dimensions_constellation")
}


#' @rdname purge_dimensions_constellation
#' @export
purge_dimensions_constellation.constellation <- function(ct) {
  for (s in seq_along(ct$star)) {
    ct$star[[s]] <- purge_dimensions_star_schema(ct$star[[s]])
  }
  ct$dimension <- vector("list")
  conform_all_dimensions(ct)
}


