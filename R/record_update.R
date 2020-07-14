#' record_update S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' For a dimension, it relates old record field values to the new values to
#' replace them.
#'
#' @return A `record_update` object.
#'
#' @keywords internal
new_record_update <- function(dimension, old, new) {
  update <- list(dimension = dimension,
                 old = old,
                 new = new)

  structure(update,
            class = "record_update")
}
