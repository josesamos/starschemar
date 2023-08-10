
#' Make a dimension record equal to another
#'
#' For a dimension, given the primary key of two records, it adds an update to
#' the set of updates that modifies the combination of values of the rest of
#' attributes of the first record so that they become the same as those of the
#' second.
#'
#' Primary keys are only used to get the combination of values easily. The
#' update is defined exclusively from the rest of values.
#'
#' It is especially useful when it is detected that two records should be only
#' one: Two have been generated due to some data error.
#'
#' @param updates A `record_update_set` object.
#' @param dimension A `dimension_table` object, dimension to update.
#' @param old A number, primary key of the record to update.
#' @param new A number, primary key of the record from which the values are
#'   taken.
#'
#' @return A `record_update_set` object.
#'
#' @family data cleaning functions
#'
#' @examples
#'
#' library(tidyr)
#'
#' dim_names <- st_mrs_age %>%
#'     get_dimension_names()
#'
#' where <- st_mrs_age %>%
#'   get_dimension("where")
#'
#' # head(where, 2)
#'
#' updates <- record_update_set() %>%
#'   match_records(dimension = where,
#'                 old = 1,
#'                 new = 2)
#'
#' @export
match_records <- function(updates, dimension, old, new) {
  UseMethod("match_records")
}


#' @rdname match_records
#' @export
match_records.record_update_set <-
  function(updates, dimension, old, new) {
    stopifnot(!is_role_playing_dimension(dimension))
    dim_txt <- dimension
    dim_txt[, -1] <- prepare_join(dim_txt[, -1])
    dru <- new_record_update(
      dimension = attr(dimension, "name"),
      old = unlist(dim_txt[old,-1]),
      new = unlist(dim_txt[new,-1])
    )
    class <- class(updates)
    updates <- c(updates, list(dru))
    class(updates) <- class
    updates
  }
