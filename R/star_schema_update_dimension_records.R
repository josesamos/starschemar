#' Apply dimension record update operations
#'
#' Given a list of dimension record update operations, they are applied on the
#' dimensions of the `star_schema` object. Update operations must be defined
#' with the set of functions available for that purpose.
#'
#' When dimensions are defined, records can be detected that must be modified as
#' part of the data cleaning process: frequently to unify two or more records
#' due to data errors or missing data. This is not immediate because facts must
#' be adapted to the new set of dimension instances.
#'
#' This operation allows us to unify records and automatically propagate
#' modifications to facts.
#'
#' The list of update operations can be applied repeatedly to new data received
#' to be incorporated into the star_schema object.
#'
#' @param st A `star_schema` object.
#' @param updates List of dimension record update operations to apply.
#'
#' @return A `star_schema` object.
#'
#' @family star schema functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
update_dimension_records <-
  function(st, updates = record_update_set()) {
    UseMethod("update_dimension_records")
  }


#' @rdname update_dimension_records
#' @export
update_dimension_records.star_schema <-
  function(st, updates = record_update_set()) {
    dimensions <- get_all_dimensions(st)
    mod_dim <- update_dimensions(updates, dimensions)
    st <- update_facts_with_dimensions(st, mod_dim)
    st
  }


#' Title
#'
#' @param st
#'
#' @return
#' @keywords internal
#' @noRd
#'
get_all_dimensions <- function(st) {
  names <- get_dimension_names(st)
  dimensions <- st$dimension[names]
  for (n in names) {
    dimensions[[n]] <- get_dimension(st, n)
  }
  dimensions
}
