
#' Modify dimensions in the list
#'
#' Apply dimension record update operations to the dimensions in the list.
#' Returns the list of modified dimensions.
#'
#' @param updates A `record_update_set` object, list of dimension record update
#'   operations to apply.
#' @param dimensions List of `dimension_table` objects to update.
#'
#' @return List of updated `dimension_table` objects.
#'
#' @examples
#'
#' @keywords internal
update_dimensions <- function(updates, dimensions) {
  UseMethod("update_dimensions")
}


#' @rdname update_dimensions
#' @export
#' @keywords internal
update_dimensions.record_update_set <-
  function(updates, dimensions) {
    mod_dim <- list()
    for (d in seq_along(dimensions)) {
      name <- get_dimension_name(dimensions[[d]])
      if (dimension_in_updates(updates, name)) {
        dim <- update_dimension(dimensions[[d]], updates)
        mod_dim_names <- names(mod_dim)
        mod_dim <- c(mod_dim, list(dim))
        names(mod_dim) <- c(mod_dim_names, name)
      }
    }
    mod_dim
  }
