
#' Query dimension in a `dimensional_query` object
#'
#' To add a dimension in a `dimensional_query` object, we have to define its
#' name and a subset of the dimension attributes. If only the name of the
#' dimension is indicated, it is considered that all its attributes should be
#' added.
#'
#' @param dq A `dimensional_query` object.
#' @param name A string, name of the dimension.
#' @param attributes A vector of attribute names.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' dq <- dimensional_query(ms_mrs) %>%
#'   query_dimension(name = "where",
#'                   attributes = c("city", "state")) %>%
#'   query_dimension(name = "when")
#'
#' @export
query_dimension <- function(dq,
                            name = NULL,
                            attributes = NULL) {
  UseMethod("query_dimension")
}



#' @rdname query_dimension
#' @export
query_dimension.dimensional_query <- function(dq,
                                              name = NULL,
                                              attributes = NULL) {
  stopifnot(!is.null(name))
  stopifnot(name %in% names(dq$input$dimension))
  stopifnot(!(name %in% names(dq$dimension)))
  stopifnot(length(attributes) == length(unique(attributes)))
  all_attributes <- names(dq$input$dimension[[name]])
  key <- all_attributes[1]
  attributes_defined <- all_attributes[-1]
  for (attribute in attributes) {
    stopifnot(attribute %in% attributes_defined)
  }
  if (is.null(attributes) |
      setequal(attributes, attributes_defined)) {
    attributes <- attributes_defined
  }
  attributes <- c(key, attributes)
  if (is.null(dq$dimension)) {
    dq$dimension <- list(name = attributes)
    names(dq$dimension) <- name
  } else {
    dim_names <- names(dq$dimension)
    dq$dimension <- c(dq$dimension, list(name = attributes))
    names(dq$dimension) <- c(dim_names, name)
  }
  dq
}
