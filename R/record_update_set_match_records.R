
#' Make a record equal to another
#'
#' For a dimension, given the primary key of two records, it adds an update to
#' the set of updates that modifies the values of the rest of attributes of the
#' first record so that they are the same as those of the second.
#'
#' @param updates A `record_update_set` object.
#' @param dimension A string, name of the dimension to update.
#' @param old A number, primary key of the record to update.
#' @param new A number, primary key of the record from which the values are
#'   taken.
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
match_records <- function(updates, dimension, old, new) {
  UseMethod("match_records")
}


#' @rdname match_records
#' @export
match_records.record_update_set <-
  function(updates, dimension, old, new) {
    class <- class(updates)
    stopifnot(!is_role_playing_dimension(dimension))
    dim_txt <- dimension
    dim_txt[,-1] <- prepare_join(dim_txt[,-1])
    dru <- new_record_update(
      dimension = attr(dimension, "name"),
      old = unlist(dim_txt[old, -1]),
      new = unlist(dim_txt[new, -1])
    )
    updates <- c(updates, list(dru))
    class(updates) <- class
    updates
  }
