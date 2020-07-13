

#' Title
#'
#' @param ct
#' @param updates
#'
#' @return
#'
#' @family constellation functions
#' @seealso
#'
#' @examples
#'
#'
#' @export
update_conformed_dimension_records <-
  function(ct, updates = record_update_set()) {
    UseMethod("update_conformed_dimension_records")
  }


#' @rdname update_conformed_dimension_records
#' @export
update_conformed_dimension_records.constellation <-
  function(ct, updates = record_update_set()) {
    mod_dim <- update_dimensions(updates, ct$dimension)

    for (s in seq_along(ct$star)) {
      ct$star[[s]] <- update_facts_with_dimensions(ct$star[[s]], mod_dim)
    }
    dimensions <- names(ct$dimension)
    ct$dimension <- list()
    for (d in dimensions) {
      ct <- conform_dimensions(ct, dim_name = d)
    }
    ct
  }
