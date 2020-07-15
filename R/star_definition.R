#' star_definition S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @return A `star_definition` object.
#'
#' @keywords internal
new_star_definition <- function() {
    schema <- list(fact = NULL, dimension = NULL)

    structure(
      schema,
      class = "star_definition"
    )
  }


#' `star_definition` S3 class
#'
#' An empty `star_definition` object is created in which definition of facts and
#' dimensions can be added.
#'
#' To get a star schema (a `star_schema` object) we need a flat table (implemented
#' through a `tibble`) and a `star_definition` object. The definition of facts and
#' dimensions in the `star_definition` object is made from the flat table columns.
#'
#' @return A `star_definition` object.
#'
#' @family star definition functions
#' @seealso \code{\link{star_schema}}
#'
#' @examples
#'
#' sd <- star_definition()
#'
#' @export
star_definition <- function() {
  new_star_definition()
}
