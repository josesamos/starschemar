
#' Title
#'
#' @param updates A `record_update_set` object.
#' @param dimension A string, name of the dimension to update.
#' @param old A number, primary key of the record to modify.
#' @param values A vector of values.
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
update_record <-
  function(updates = NULL,
           dim,
           old,
           values = vector()) {
    UseMethod("update_record")
  }


#' @rdname update_record
#' @export
update_record.record_update_set <-
  function(updates = NULL,
           dim,
           old,
           values = vector()) {
    stopifnot(!is_role_playing_dimension(dim))
    if (is.null(updates)) {
      updates <- list()
    }
    dim_txt <- dim
    dim_txt[,-1] <- prepare_join(dim_txt[,-1])
    old_values <- unlist(dim_txt[old, -1])
    stopifnot(length(old_values) == length(values))
    names(values) <- names(old_values)
    c(updates, list(list(
      dimension = attr(dim, "name"),
      old = old_values,
      new = values
    )))
  }

