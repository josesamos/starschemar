
#' Update dimension records with a set of values
#'
#' For a dimension, given a vector of column names, a vector of old values and a
#' vector of new values, it adds an update to the set of updates that modifies
#' all the records that have the combination of old values in the columns with
#' the new values in those same columns.
#'
#' @param updates A `record_update_set` object.
#' @param dimension A `dimension_table` object, dimension to update.
#' @param columns A vector of column names.
#' @param old_values A vector of character values.
#' @param new_values A vector of character values.
#'
#' @return A `record_update_set` object.
#'
#' @family data cleaning functions
#' @seealso
#'
#' @examples
#'
#' library(tidyr)
#'
#' (dim_names <- st_mrs_age %>%
#'     get_dimension_names())
#'
#' where <- st_mrs_age %>%
#'   get_dimension("where")
#'
#' head(where, 2)
#'
#' updates <- record_update_set() %>%
#'   update_selection(
#'     dimension = where,
#'     columns = c("city"),
#'     old_values = c("Bridgepor"),
#'     new_values = c("Bridgeport")
#'   )
#'
#' @export
update_selection <-
  function(updates = NULL,
           dimension,
           columns = vector(),
           old_values = vector(),
           new_values = vector()) {
    UseMethod("update_selection")
  }


#' @rdname update_selection
#' @export
update_selection.record_update_set <-
  function(updates = NULL,
           dimension,
           columns = vector(),
           old_values = vector(),
           new_values = vector()) {
    stopifnot(!is_role_playing_dimension(dimension))
    stopifnot(length(columns) == length(old_values) &
                length(columns) == length(new_values))
    dim_col <- names(dimension)[-1]
    for (n in columns) {
      stopifnot(n %in% dim_col)
    }
    dim_txt <- dimension
    dim_txt[, -1] <- prepare_join(dim_txt[, -1])
    names(old_values) <- columns
    names(new_values) <- columns
    dru <- new_record_update(
      dimension = attr(dimension, "name"),
      old = old_values,
      new = new_values
    )
    class <- class(updates)
    updates <- c(updates, list(dru))
    class(updates) <- class
    updates
  }
