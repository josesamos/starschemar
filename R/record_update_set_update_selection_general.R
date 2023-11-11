
#' Update dimension records with a set of values in given columns
#'
#' For a dimension, given a vector of column names, a vector of old values for
#' those columns, another vector column names, and a vector of new values for
#' those columns, it adds an update to the set of updates that modifies all the
#' records that have the combination of old values in the first column vector
#' with the new values in the second column vector.
#'
#' @param updates A `record_update_set` object.
#' @param dimension A `dimension_table` object, dimension to update.
#' @param columns_old A vector of column names.
#' @param old_values A vector of character values.
#' @param columns_new A vector of column names.
#' @param new_values A vector of character values.
#'
#' @return A `record_update_set` object.
#'
#' @family data cleaning functions
#'
#' @examples
#'
#' dim_names <- st_mrs_age |>
#'     get_dimension_names()
#'
#' where <- st_mrs_age |>
#'   get_dimension("where")
#'
#' # head(where, 2)
#'
#' updates <- record_update_set() |>
#'   update_selection_general(
#'     dimension = where,
#'     columns_old = c("state", "city"),
#'     old_values = c("CT", "Bridgepor"),
#'     columns_new = c("city"),
#'     new_values = c("Bridgeport")
#'   )
#'
#' @export
update_selection_general <-
  function(updates = NULL,
           dimension,
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
           dimension,
           columns_old = vector(),
           old_values = vector(),
           columns_new = vector(),
           new_values = vector()) {
    stopifnot("The dimension is a role playing dimension." = !is_role_playing_dimension(dimension))
    dim_col <- names(dimension)[-1]
    for (n in unique(c(columns_old, columns_new))) {
      validate_names(dim_col, n, concept = 'column')
    }
    dim_txt <- dimension
    dim_txt[, -1] <- prepare_join(dim_txt[, -1])
    names(old_values) <- columns_old
    names(new_values) <- columns_new
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
