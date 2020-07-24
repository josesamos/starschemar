

#' Apply dimension record update operations to conformed dimensions
#'
#' Given a list of dimension record update operations, they are applied on the conformed
#' dimensions of the `constellation` object. Update operations must be defined
#' with the set of functions available for that purpose.
#'
#' When dimensions are defined, records can be detected that must be modified as
#' part of the data cleaning process: frequently to unify two or more records
#' due to data errors or missing data. This is not immediate because facts must
#' be adapted to the new set of dimension instances.
#'
#' This operation allows us to unify records and automatically propagate
#' modifications to facts in star schemas.
#'
#' @param ct A `constellation` object.
#' @param updates A `record_update_set` object.
#'
#' @return A `constellation` object.
#'
#' @family data cleaning functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' ct <- ct_mrs %>%
#'   modify_conformed_dimension_records(updates_st_mrs_age)
#'
#' @export
modify_conformed_dimension_records <-
  function(ct, updates = record_update_set()) {
    UseMethod("modify_conformed_dimension_records")
  }


#' @rdname modify_conformed_dimension_records
#' @export
modify_conformed_dimension_records.constellation <-
  function(ct, updates = record_update_set()) {
    mod_dim <- update_dimensions(ct$dimension, updates)

    for (s in seq_along(ct$star)) {
      ct$star[[s]] <- update_facts_with_dimensions(ct$star[[s]], mod_dim)
    }
    dimensions <- names(ct$dimension)
    ct$dimension <- list()
    for (d in dimensions) {
      ct <- conform_dimensions(ct, name = d)
    }
    ct
  }
