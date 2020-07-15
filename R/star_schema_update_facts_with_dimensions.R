
#' Title
#'
#' @param st
#' @param mod_dim
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
update_facts_with_dimensions <- function(st, mod_dim) {
  UseMethod("update_facts_with_dimensions")
}


#' @rdname update_facts_with_dimensions
#' @export
#' @keywords internal
update_facts_with_dimensions.star_schema <- function(st, mod_dim) {
  name_dims <- get_dimension_names(st)
  for (mod_d in seq_along(mod_dim)) {
    dim_name <- names(mod_dim)[mod_d]
    if (dim_name %in% name_dims) {
      dim <-
        new_dimension_table(tibble::as_tibble(mod_dim[[mod_d]][,-1]), name = dim_name)
      if (!is_role_dimension(st$dimension[[dim_name]])) {
        st <-
          update_facts_with_general_dimension(st, dim_name, mod_dim[[mod_d]], dim)
      } else {
        st <-
          update_facts_with_role_dimension(st, name_dims, dim_name, mod_dim[[mod_d]], dim)
      }
    }
  }
  st$fact[[1]] <- group_table(st$fact[[1]])
  st
}



#' Title
#'
#' @param st
#' @param dim_name
#' @param old_dim
#' @param dim
#'
#' @return
#' @keywords internal
#' @noRd
#'
update_facts_with_general_dimension <- function(st, dim_name, old_dim, dim) {
  st$fact[[1]] <-
    dereference_dimension(st$fact[[1]], old_dim)
  type = get_dimension_type(st$dimension[[dim_name]])
  st$dimension[[dim_name]] <- dim
  st$dimension[[dim_name]] <-
    set_dimension_type(st$dimension[[dim_name]], type)
  st$fact[[1]] <-
    reference_dimension(st$fact[[1]], dim, names(dim)[-1])
  st
}


#' Title
#'
#' @param st
#' @param name_dims
#' @param dim_name
#' @param old_dim
#' @param dim
#'
#' @return
#' @keywords internal
#' @noRd
#'
update_facts_with_role_dimension <-
  function(st, name_dims, dim_name, old_dim, dim) {
    rp_dim_name <-
      get_role_playing_dimension_name(st$dimension[[dim_name]])
    tmp <- names(st$dimension[[rp_dim_name]])
    st$dimension[[rp_dim_name]] <- old_dim # dimension with matches
    names(st$dimension[[rp_dim_name]]) <- tmp

    for (d in name_dims) {
      if (get_role_playing_dimension_name(st$dimension[[d]]) == rp_dim_name) {
        od <- get_dimension(st, d)
        st$fact[[1]] <-
          dereference_dimension(st$fact[[1]], od)
      }
    }

    dim <- set_dimension_type_role_playing(dim)
    dim <- set_dimension_name(dim, rp_dim_name)
    names(dim) <- names(st$dimension[[rp_dim_name]])
    st$dimension[[rp_dim_name]] <- dim # dimension with unique rows

    for (d in name_dims) {
      if (get_role_playing_dimension_name(st$dimension[[d]]) == rp_dim_name) {
        nd <- get_dimension(st, d)
        st$fact[[1]] <-
          reference_dimension(st$fact[[1]], nd, names(nd)[-1])
      }
    }
    st
  }


