#' record_update_set S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @return A `record_update_set` object.
#'
#' @keywords internal
new_record_update_set <- function() {
  update_set <- list()

  structure(update_set,
            class = "record_update_set")
}


#' `record_update_set` S3 class
#'
#' A `record_update_set` object is created. Stores updates on dimension records.
#'
#' Each update is made up of a dimension name, an old value set, and a new value
#' set.
#'
#' When the update is applied, all the dimension records that have the
#' combination of old values are modified with the new values provided.
#'
#' @return A `record_update_set` object.
#'
#' @family dimension update definition functions
#' @seealso
#'
#' @examples
#'
#' updates <- record_update_set()
#'
#' @export
record_update_set <- function() {
  new_record_update_set()
}
