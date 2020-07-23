
#' Update facts with a list of modified dimensions
#'
#' Update the fact table with the modified dimensions. New dimensions are
#' generated from the modified ones.
#'
#' @param st A `star_schema` object.
#' @param dimensions A list of `dimension_table` objects.
#'
#' @return A `star_schema` object.
#'
#' @keywords internal
update_facts_with_dimensions <- function(st, dimensions) {
  UseMethod("update_facts_with_dimensions")
}


#' @rdname update_facts_with_dimensions
#' @export
#' @keywords internal
update_facts_with_dimensions.star_schema <-
  function(st, dimensions) {
    dimension_names <- get_dimension_names(st)
    for (mod_d in seq_along(dimensions)) {
      name <- names(dimensions)[mod_d]
      if (name %in% dimension_names) {
        dim <-
          new_dimension_table(tibble::as_tibble(dimensions[[mod_d]][, -1]), name = name)
        if (is_role_dimension(st$dimension[[name]])) {
          st <-
            update_facts_with_role_dimension(st, name, dimensions[[mod_d]], dim, dimension_names)
        } else {
          st <-
            update_facts_with_general_dimension(st, name, dimensions[[mod_d]], dim)
        }
      }
    }
    st$fact[[1]] <- group_table(st$fact[[1]])
    st
  }



#' Update facts with a general dimension
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#' @param old_dimension A `dimension_table` object.
#' @param dimension A `dimension_table` object.
#'
#' @return A `star_schema` object.
#' @keywords internal
update_facts_with_general_dimension <-
  function(st, name, old_dimension, dimension) {
    st$fact[[1]] <-
      dereference_dimension(st$fact[[1]], old_dimension)
    type = get_dimension_type(st$dimension[[name]])
    st$dimension[[name]] <- dimension
    st$dimension[[name]] <-
      set_dimension_type(st$dimension[[name]], type)
    st$fact[[1]] <-
      reference_dimension(st$fact[[1]], dimension, names(dimension)[-1])
    st
  }


#' Update facts with a role dimension
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#' @param old_dimension A `dimension_table` object.
#' @param dimension A `dimension_table` object.
#' @param dimension_names A vector of dimension names.
#'
#' @return A `star_schema` object.
#' @keywords internal
update_facts_with_role_dimension <-
  function(st,
           name,
           old_dimension,
           dimension,
           dimension_names) {
    rp_name <-
      get_role_playing_dimension_name(st$dimension[[name]])
    tmp <- names(st$dimension[[rp_name]])
    st$dimension[[rp_name]] <-
      old_dimension # dimension with matches
    names(st$dimension[[rp_name]]) <- tmp

    for (d in dimension_names) {
      if (get_role_playing_dimension_name(st$dimension[[d]]) == rp_name) {
        od <- get_dimension(st, d)
        st$fact[[1]] <-
          dereference_dimension(st$fact[[1]], od)
      }
    }

    dimension <- set_dimension_type_role_playing(dimension)
    dimension <- set_dimension_name(dimension, rp_name)
    names(dimension) <- names(st$dimension[[rp_name]])
    st$dimension[[rp_name]] <-
      dimension # dimension with unique rows

    for (d in dimension_names) {
      if (get_role_playing_dimension_name(st$dimension[[d]]) == rp_name) {
        nd <- get_dimension(st, d)
        st$fact[[1]] <-
          reference_dimension(st$fact[[1]], nd, names(nd)[-1])
      }
    }
    st
  }
