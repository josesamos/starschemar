
#' Title
#'
#' @param st
#' @param dim_name
#'
#' @return
#'
#' @family star schema functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' @export
get_dimension <- function(st, dim_name) {
  UseMethod("get_dimension")
}


#' @rdname get_dimension
#' @export
get_dimension.star_schema <- function(st, dim_name) {
  dim <- NULL
  if (dim_name %in% names(st$dimension)) {
    dim <- st$dimension[[dim_name]]
    if (is_role_dimension(dim)) {
      type <- get_dimension_type(dim)
      rp_name <- get_role_playing_dimension_name(dim)
      shared_dim <- st$dimension[[rp_name]]
      names(shared_dim) <- names(dim)  # role dim. only stores names
      dim <- set_dimension_name(shared_dim, dim_name)
      dim <- set_dimension_type(dim, type)
      dim <- set_role_playing_dimension_name(dim, rp_name)
    } else if (is_role_playing_dimension(dim)) {
      dim <- NULL
    }
  }
  dim
}
