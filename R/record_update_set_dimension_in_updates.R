
#' Dimension in set of updates
#'
#' Given a set of dimension record update operations and the name of a
#' dimension, it checks if there is any update operation to perform on the
#' dimension.
#'
#' @param updates A `record_update_set` object, list of dimension record update
#'   operations.
#' @param name A string, name of the dimension.
#'
#' @return A boolean, indicating if the dimension appears in the list of update
#'   operations.
#' @keywords internal
#'
#' @examples
#'
#' @keywords internal
dimension_in_updates <- function(updates, name) {
  UseMethod("dimension_in_updates")
}


#' @rdname dimension_in_updates
#' @export
#' @keywords internal
dimension_in_updates.record_update_set <- function(updates, name) {
  for (m in seq_along(updates)) {
    match <- updates[[m]]
    if (match$dimension == name) {
      return(TRUE)
    }
  }
  FALSE
}


