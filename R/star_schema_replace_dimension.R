

#' Title
#'
#' @param st
#' @param dim_name
#' @param dim
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
replace_dimension <- function(st, dim_name, dim) {
  UseMethod("replace_dimension")
}


#' @rdname replace_dimension
#' @export
#' @keywords internal
replace_dimension.star_schema <- function(st, dim_name, dim) {
  if (!is_role_dimension(st$dimension[[dim_name]])) {
    st$dimension[[dim_name]] <- dim
  } else {
    rp_dim_name <-
      get_role_playing_dimension_name(st$dimension[[dim_name]])
    dim <- set_role_playing_dimension_type(dim)
    names(dim) <- names(st$dimension[[rp_dim_name]])
    st$dimension[[rp_dim_name]] <- dim
  }
  st
}
