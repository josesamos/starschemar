
#' Update a dimension record with a set of values
#'
#' For a dimension, given the primary key of one record, it adds an update to
#' the set of updates that modifies the combination of values of the rest of
#' attributes of the selected record so that they become those given.
#'
#' Primary key is only used to get the combination of values easily. The
#' update is defined exclusively from the rest of values.
#'
#' @param updates A `record_update_set` object.
#' @param dimension A `dimension_table` object, dimension to update.
#' @param old A number, primary key of the record to modify.
#' @param values A vector of character values.
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
#'   update_record(
#'     dimension = where,
#'     old = 1,
#'     values = c("1", "CT", "Bridgeport")
#'   )
#'
#' @export
update_record <-
  function(updates = NULL,
           dimension,
           old,
           values = vector()) {
    UseMethod("update_record")
  }


#' @rdname update_record
#' @export
update_record.record_update_set <-
  function(updates = NULL,
           dimension,
           old,
           values = vector()) {
    stopifnot(!is_role_playing_dimension(dimension))
    dim_txt <- dimension
    dim_txt[, -1] <- prepare_join(dim_txt[, -1])
    old_values <- unlist(dim_txt[old,-1])
    stopifnot(length(old_values) == length(values))
    names(values) <- names(old_values)
    dru <- new_record_update(
      dimension = attr(dimension, "name"),
      old = old_values,
      new = values
    )
    class <- class(updates)
    updates <- c(updates, list(dru))
    class(updates) <- class
    updates
  }

