

# replace_dimension -------------------------------------------------------


#' Replace a star schema dimension
#'
#' Replace dimension with another that contains all the instances of the first
#' and, possibly, some more, in a star schema.
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#' @param dimension A `dimension_table` object.
#'
#' @return A `star_schema` object.
#'
#' @keywords internal
replace_dimension <- function(st, name, dimension) {
  UseMethod("replace_dimension")
}


#' @rdname replace_dimension
#' @export
#' @keywords internal
replace_dimension.star_schema <- function(st, name, dimension) {
  if (is_role_dimension(st$dimension[[name]])) {
    rp_name <- get_role_playing_dimension_name(st$dimension[[name]])
    dimension <- set_dimension_type_role_playing(dimension)
    names(dimension) <- names(st$dimension[[rp_name]])
    st$dimension[[rp_name]] <- dimension
  } else {
    st$dimension[[name]] <- dimension
  }
  st
}

# replace_dimension_in_facts ----------------------------------------------


#' Replace in facts a star schema dimension
#'
#' This operation can be due to integrating several dimensions in a
#' constellation or an incremental update of a dimension (indicated with the
#' boolean parameter). The new dimension replaces in facts the original
#' dimension, whose name is indicated.
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#' @param dimension A `dimension_table` object.
#' @param set_type_conformed A boolean.
#'
#' @return A `star_schema` object.
#'
#' @keywords internal
replace_dimension_in_facts <-
  function(st, name, dimension, set_type_conformed = FALSE) {
    UseMethod("replace_dimension_in_facts")
  }


#' @rdname replace_dimension_in_facts
#' @export
#' @keywords internal
replace_dimension_in_facts.star_schema <-
  function(st, name, dimension, set_type_conformed = FALSE) {
    dimension_names <- get_dimension_names(st)
    if (name %in% dimension_names) {
      if (is_role_dimension(st$dimension[[name]])) {
        st <-
          replace_role_dimension_in_facts(st, name, dimension, dimension_names)
      } else {
        st <- replace_general_dimension_in_facts(st, name, dimension)
      }
      if (set_type_conformed) {
        st$dimension[[name]] <-
          set_dimension_type_conformed(st$dimension[[name]])
      }
    }
    st
  }



#' Replace in facts a star schema general dimension
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#' @param dimension A `dimension_table` object.
#'
#' @return A `star_schema` object.
#' @keywords internal
replace_general_dimension_in_facts <- function(st, name, dimension) {
  st$fact[[1]] <-
    dereference_dimension(st$fact[[1]], st$dimension[[name]])
  st$dimension[[name]] <- dimension
  st$fact[[1]] <-
    reference_dimension(st$fact[[1]], dimension, names(dimension)[-1])
  st
}


#' Replace in facts a star schema role dimension
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#' @param dimension A `dimension_table` object.
#' @param dimension_names A vector of dimension names.
#'
#' @return A `star_schema` object.
#' @keywords internal
replace_role_dimension_in_facts <-
  function(st,
           name,
           dimension,
           dimension_names) {
    rp_name <-
      get_role_playing_dimension_name(st$dimension[[name]])
    for (d in dimension_names) {
      if (get_role_playing_dimension_name(st$dimension[[d]]) == rp_name) {
        old_dim <- get_dimension(st, d)
        st$fact[[1]] <-
          dereference_dimension(st$fact[[1]], old_dim)
      }
    }

    dimension <- set_dimension_type_role_playing(dimension)
    names(dimension) <- names(st$dimension[[rp_name]])
    st$dimension[[rp_name]] <- dimension

    for (d in dimension_names) {
      if (get_role_playing_dimension_name(st$dimension[[d]]) == rp_name) {
        new_dim <- get_dimension(st, d)
        st$fact[[1]] <-
          reference_dimension(st$fact[[1]], new_dim, names(new_dim)[-1])
      }
    }
    st
  }


