#' dimensional_model S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @return A `dimensional_model` object.
#'
#' @keywords internal
new_dimensional_model <- function() {
    schema <- list(fact = NULL, dimension = NULL)

    structure(
      schema,
      class = "dimensional_model"
    )
  }


#' `dimensional_model` S3 class
#'
#' An empty `dimensional_model` object is created in which definition of facts
#' and dimensions can be added.
#'
#' To get a star schema (a `star_schema` object) we need a flat table
#' (implemented through a `tibble`) and a `dimensional_model` object. The
#' definition of facts and dimensions in the `dimensional_model` object is made
#' from the flat table columns. Each attribute can only appear once in the
#' definition.
#'
#' @return A `dimensional_model` object.
#'
#' @family star definition functions
#' @seealso \code{\link{star_schema}}
#'
#' @examples
#'
#' dm <- dimensional_model()
#'
#' @export
dimensional_model <- function() {
  new_dimensional_model()
}


#' Get attribute names
#'
#' Get the names of the attributes used so far in the definition.
#'
#' @param dm A `dimensional_model` object.
#'
#' @return A vector of attribute names.
#' @keywords internal
get_attribute_names <- function(dm) {
  c(dm$fact$measures, dm$fact$nrow_agg, unlist(dm$dimension, use.names=FALSE))
}
