#' dimensional_query S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @return A `dimensional_query` object.
#'
#' @keywords internal
new_dimensional_query <- function(multistar = NULL) {
  schema <-
    list(
      fact = NULL,
      dimension = NULL,
      key = NULL,
      input = multistar,
      output = NULL
    )

  structure(schema,
            class = "dimensional_query")
}


#' `dimensional_query` S3 class
#'
#' An empty `dimensional_query` object is created where you can select fact
#' measures, dimension attributes and filter dimension rows.
#'
#' @param multistar A `multistar` object.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' ms_mrs <- ct_mrs %>%
#'   constellation_as_multistar()
#'
#' dq <- dimensional_query(ms_mrs)
#'
#' @export
dimensional_query <- function(multistar = NULL) {
  new_dimensional_query(multistar)
}
