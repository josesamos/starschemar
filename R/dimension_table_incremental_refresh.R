

#' Title
#'
#' @param dim
#' @param dim_new
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
incremental_refresh_dimension <- function(dim, dim_new) {
  UseMethod("incremental_refresh_dimension")
}


#' @rdname incremental_refresh_dimension
#' @export
#' @keywords internal
incremental_refresh_dimension.dimension_table <-
  function(dim, dim_new) {
    tmp <-
      dplyr::right_join(dim, dim_new[,-1], by = names(dim_new[,-1]))
    tmp <- tmp[is.na(tmp[, 1]),]
    if (nrow(tmp) > 0) {
      tmp[, 1] <- max(dim[, 1]) + (1:nrow(tmp))
      class <- attr(dim, "class")
      dim <-
        tibble::as_tibble(dplyr::bind_rows(as.data.frame(dim), as.data.frame(tmp)))
      attr(dim, "class") <- class
    }
    dim
  }
