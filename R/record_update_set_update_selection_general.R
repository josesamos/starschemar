
#' Title
#'
#' @param updates A `record_update_set` object.
#' @param dimension A string, name of the dimension to update.
#' @param columns_old A vector of column names.
#' @param old_values A vector of values.
#' @param columns_new A vector of column names.
#' @param new_values A vector of values.
#'
#' @return A `record_update_set` object.
#'
#' @family dimension update functions
#' @seealso
#'
#' @examples
#'
#'
#' @export
update_selection_general <-
  function(updates = NULL,
           dim,
           columns_old = vector(),
           old_values = vector(),
           columns_new = vector(),
           new_values = vector()) {
    UseMethod("update_selection_general")
  }


#' @rdname update_selection_general
#' @export
update_selection_general.record_update_set <-
  function(updates = NULL,
           dim,
           columns_old = vector(),
           old_values = vector(),
           columns_new = vector(),
           new_values = vector()) {
    stopifnot(!is_role_playing_dimension(dim))
    dim_col <- names(dim)[-1]
    for (n in unique(c(columns_old, columns_new))) {
      stopifnot(n %in% dim_col)
    }
    if (is.null(updates)) {
      updates <- list()
    }
    dim_txt <- dim
    dim_txt[, -1] <- prepare_join(dim_txt[, -1])
    names(old_values) <- columns_old
    names(new_values) <- columns_new
    c(updates, list(list(
      dimension = attr(dim, "name"),
      old = old_values,
      new = new_values
    )))
  }
