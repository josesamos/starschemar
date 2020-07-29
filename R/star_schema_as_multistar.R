
#' Export a star schema as a `multistar`
#'
#' Once we have refined the format or content of facts and dimensions, we can
#' obtain a `multistar`. A `multistar` only distinguishes between general and
#' conformed dimensions, each dimension has its own data. It can contain
#' multiple fact tables.
#'
#' @param st A `star_schema` object.
#'
#' @return A `multistar` object.
#'
#' @family results export functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' ms <- st_mrs_age %>%
#'   star_schema_as_multistar()
#'
#' @export
star_schema_as_multistar <- function(st) {
  UseMethod("star_schema_as_multistar")
}


#' @rdname star_schema_as_multistar
#' @export
star_schema_as_multistar.star_schema <- function(st) {
  star_schema_as_mst(st)
}


# Star schema as multistar export (common) --------------------------------

#' Star schema as `multistar` export (common)
#'
#' @param st A `star_schema` object.
#' @param fl A list of `fact_table` objects.
#' @param dl A list of `dimension_table` objects.
#' @param commondim A list of dimension names already included.
#'
#' @return A `multistar` object.
#' @keywords internal
star_schema_as_mst <- function(st,
                              fl = NULL,
                              dl = NULL,
                              commondim = NULL) {
  UseMethod("star_schema_as_mst")
}


#' @rdname star_schema_as_mst
#' @export
#' @keywords internal
star_schema_as_mst.star_schema <- function(st,
                                          fl = NULL,
                                          dl = NULL,
                                          commondim = NULL) {
  fl_names <- names(fl)
  fl <- c(fl, list(st$fact[[1]]))
  names(fl) <- c(fl_names, attr(st$fact[[1]], "name"))

  dim <- get_all_dimensions(st)
  dl_names <- names(dl)
  for (d in seq_along(dim)) {
    name_dim <- attr(dim[[d]], "name")
    if (!(name_dim %in% commondim)) {
      dl <- c(dl, list(dim[[d]]))
      dl_names <- c(dl_names, name_dim)
    }
  }
  names(dl) <- dl_names
  new_multistar(fl, dl)
}
