#' record_update S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @return A `record_update` object.
#'
new_record_update <- function(dimension, old, new) {
  update <- list(
    dimension = dimension,
    old = old,
    new = new
  )

  structure(update,
            class = "record_update")
}


#' `record_update` S3 class
#'
#' A `record_update` object is created.
#'
#' @return A `record_update` object.
#' @export
#'
#' @family dimension update functions
#' @seealso
#'
#' @examples
#'
#'
record_update <- function(dimension, old, new) {
  new_record_update(dimension, old, new)
}
