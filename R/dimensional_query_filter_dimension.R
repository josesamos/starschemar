
#' Filter dimension
#'
#' Allows you to define selection conditions for dimension rows.
#'
#' Conditions can be defined on any attribute of the dimension (not only on
#' attributes selected in the query for the dimension). The selection is made
#' based on the function `dplyr::filter`. Conditions are defined in exactly the
#' same way as in that function.
#'
#' @param dq A `dimensional_query` object.
#' @param name A string, name of the dimension.
#' @param ... Conditions, defined in exactly the same way as in `dplyr::filter`.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   filter_dimension(name = "when", when_happened_week <= "03") |>
#'   filter_dimension(name = "where", city == "Boston")
#'
#' @export
filter_dimension <- function(dq,
                            name = NULL,
                            ...) {
  UseMethod("filter_dimension")
}



#' @rdname filter_dimension
#' @export
filter_dimension.dimensional_query <- function(dq,
                                               name = NULL,
                                               ...) {
  stopifnot(!is.null(name))
  stopifnot(name %in% names(dq$input$dimension))
  stopifnot(!(name %in% names(dq$key)))
  key <- dplyr::filter(tibble::as_tibble(dq$input$dimension[[name]]), ...)[[1]]
  if (is.null(dq$key)) {
    dq$key <- list(name = key)
    names(dq$key) <- name
  } else {
    dim_names <- names(dq$key)
    dq$key <- c(dq$key, list(name = key))
    names(dq$key) <- c(dim_names, name)
  }
  dq
}
