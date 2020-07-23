
#' Incrementally refresh a dimension with another
#'
#' Incrementally refresh a dimension with the content of a new one that is
#' integrated into the first.
#'
#' @param dimension A `dimension_table` object.
#' @param dimension_new A `dimension_table` object, possibly with new data.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
incremental_refresh_dimension <-
  function(dimension, dimension_new) {
    UseMethod("incremental_refresh_dimension")
  }


#' @rdname incremental_refresh_dimension
#' @export
#' @keywords internal
incremental_refresh_dimension.dimension_table <-
  function(dimension, dimension_new) {
    tmp <-
      dplyr::right_join(dimension, dimension_new[, -1], by = names(dimension_new[, -1]))
    tmp <- tmp[as.vector(is.na(tmp[, 1])), ]
    if (nrow(tmp) > 0) {
      tmp[, 1] <- max(dimension[, 1]) + (1:nrow(tmp))
      class <- attr(dimension, "class")
      dimension <-
        tibble::as_tibble(dplyr::bind_rows(as.data.frame(dimension), as.data.frame(tmp)))
      attr(dimension, "class") <- class
    }
    dimension
  }
