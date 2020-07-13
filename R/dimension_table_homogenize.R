

#' Title
#'
#' @param dim
#' @param attributes
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
homogenize <- function(dim, attributes = NULL) {
  UseMethod("homogenize")
}


#' @rdname homogenize
#' @export
#' @keywords internal
homogenize.dimension_table <- function(dim, attributes = NULL) {
  dim <- dim[,-1]
  if (!is.null(attributes)) {
    names(dim) <- attributes
  }
  dim
}
