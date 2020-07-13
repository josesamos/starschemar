

#' Title
#'
#' @param ct A `constellation` object.
#' @param dim_name
#'
#' @return A `constellation` object.
#'
#' @examples
#'
#' @keywords internal
conform_dimensions <- function(ct, dim_name = NULL) {
  UseMethod("conform_dimensions")
}


#' @rdname conform_dimensions
#' @export
#' @keywords internal
conform_dimensions.constellation <- function(ct, dim_name = NULL) {
  dl <- list()
  for (s in seq_along(ct$star)) {
    dim <- get_dimension(ct$star[[s]], dim_name)
    if ("dimension_table" %in% class(dim)) {
      dim <- homogenize(dim)
      dl <- c(dl, list(dim))
    }
  }
  if (length(dl) > 1) {
    d_new <-
      union_of_dimensions(dl, name = dim_name, type = "conformed")
    ct$dimension <- c(ct$dimension, list(d_new))
    names(ct$dimension)[length(ct$dimension)] <- dim_name

    for (s in seq_along(ct$star)) {
      ct$star[[s]] <- conform_dimension(ct$star[[s]], dim_name, d_new)
    }
  }
  ct
}
