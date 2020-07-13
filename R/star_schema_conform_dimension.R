
#' Title
#'
#' @param st
#' @param dim_name
#' @param dim
#' @param incremental_update
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
conform_dimension <-
  function(st, dim_name, dim, incremental_update = FALSE) {
    UseMethod("conform_dimension")
  }


#' @rdname conform_dimension
#' @export
#' @keywords internal
conform_dimension.star_schema <-
  function(st, dim_name, dim, incremental_update = FALSE) {
    name_dims <- get_dimension_names(st)
    if (dim_name %in% name_dims) {
      if (!is_role_dimension(st$dimension[[dim_name]])) {
        st <- conform_general_dimension(st, dim_name, dim)
      } else {
        st <-
          conform_role_dimension(st, name_dims, dim_name, dim, incremental_update)
      }
    }
    st
  }


#' Title
#'
#' @param st
#' @param dim_name
#' @param dim
#'
#' @return
#' @keywords internal
#' @noRd
#'
conform_general_dimension <- function(st, dim_name, dim) {
  st$fact[[1]] <-
    dereference_dimension(st$fact[[1]], st$dimension[[dim_name]])
  st$dimension[[dim_name]] <- dim
  st$fact[[1]] <-
    reference_dimension(st$fact[[1]], dim, names(dim)[-1])
  st
}



#' Title
#'
#' @param st
#' @param name_dims
#' @param dim_name
#' @param dim
#' @param incremental_update
#'
#' @return
#' @keywords internal
#' @noRd
#'
conform_role_dimension <- function(st, name_dims, dim_name, dim, incremental_update) {
  if (!incremental_update) {
    st$dimension[[dim_name]] <-
      set_dimension_type_conformed(st$dimension[[dim_name]])
  }

  rp_dim_name <-
    get_role_playing_dimension_name(st$dimension[[dim_name]])
  for (d in name_dims) {
    if (get_role_playing_dimension_name(st$dimension[[d]]) == rp_dim_name) {
      old_dim <- get_dimension(st, d)
      st$fact[[1]] <-
        dereference_dimension(st$fact[[1]], old_dim)
    }
  }

  dim <- set_role_playing_dimension_type(dim)
  names(dim) <- names(st$dimension[[rp_dim_name]])
  st$dimension[[rp_dim_name]] <- dim

  for (d in name_dims) {
    if (get_role_playing_dimension_name(st$dimension[[d]]) == rp_dim_name) {
      new_dim <- get_dimension(st, d)
      st$fact[[1]] <-
        reference_dimension(st$fact[[1]], new_dim, names(new_dim)[-1])
    }
  }
  st
}

