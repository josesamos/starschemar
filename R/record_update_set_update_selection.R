
#' Title
#'
#' @param updates A `record_update_set` object.
#' @param dimension A string, name of the dimension to update.
#' @param columns A vector of column names.
#' @param old_values A vector of values.
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
update_selection <-
  function(updates = NULL,
           dim,
           columns = vector(),
           old_values = vector(),
           new_values = vector()) {
    UseMethod("update_selection")
  }


#' @rdname update_selection
#' @export
update_selection.record_update_set <-
  function(updates = NULL,
           dim,
           columns = vector(),
           old_values = vector(),
           new_values = vector()) {
    stopifnot(!is_role_playing_dimension(dim))
    stopifnot(length(columns) == length(old_values) &
                length(columns) == length(new_values))
    dim_col <- names(dim)[-1]
    for (n in columns) {
      stopifnot(n %in% dim_col)
    }
    if (is.null(updates)) {
      updates <- list()
    }
    dim_txt <- dim
    dim_txt[, -1] <- prepare_join(dim_txt[, -1])
    names(old_values) <- columns
    names(new_values) <- columns
    c(updates, list(list(
      dimension = attr(dim, "name"),
      old = old_values,
      new = new_values
    )))
  }
