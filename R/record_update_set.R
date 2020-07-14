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
#' A `record_update_set` object is created.
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
record_update_set <- function() {
  new_record_update_set()
}
