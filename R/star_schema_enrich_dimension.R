
#' Export selected attributes of a dimension
#'
#' Export the selected attributes of a dimension, without repeated combinations,
#' to enrich the dimension.
#'
#' If it is a role dimension they cannot be exported, you have to work with the
#' associated role playing dimension.
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#' @param attributes A vector of attribute names.
#'
#' @return A `tibble` object.
#'
#' @family dimension enrichment functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' tb <-
#'   enrich_dimension_export(st_mrs_age,
#'                           name = "when_common",
#'                           attributes = c("week", "year"))
#'
#' @export
enrich_dimension_export <- function(st,
                                    name = NULL,
                                    attributes = NULL) {
  UseMethod("enrich_dimension_export")
}


#' @rdname enrich_dimension_export
#' @export
enrich_dimension_export.star_schema <- function(st,
                                                name = NULL,
                                                attributes = NULL) {
  stopifnot(!is.null(name))
  stopifnot(name %in% names(st$dimension))
  stopifnot(length(attributes) == length(unique(attributes)))
  stopifnot(length(st$dimension[[name]][[1]]) > 0)
  attributes_defined <- names(st$dimension[[name]])[-1]
  for (attribute in attributes) {
    stopifnot(attribute %in% attributes_defined)
  }
  tibble::as_tibble(unique(st$dimension[[name]][, attributes]))
}


#' Import `tibble` to enrich a dimension
#'
#' For a dimension of a star schema a `tibble` is attached. This contains
#' dimension attributes and new attributes. If values associated with all rows
#' in the dimension are included in the `tibble`, the dimension is enriched with
#' the new attributes.
#'
#' Role dimensions cannot be directly enriched. If a role playing dimension is
#' enriched, the new attributes are also added to the associated role
#' dimensions.
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#' @param tb A `tibble` object.
#'
#' @return A `star_schema` object.
#'
#' @family dimension enrichment functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' tb <-
#'   enrich_dimension_export(st_mrs_age,
#'                           name = "when_common",
#'                           attributes = c("week", "year"))
#'
#' # Add new columns with meaningful data (these are not), possibly exporting
#' # data to a file, populating it and importing it.
#' tb <- tibble::add_column(tb, x = "x", y = "y", z = "z")
#'
#' st <- enrich_dimension_import(st_mrs_age, name = "when_common", tb)
#'
#' @export
enrich_dimension_import <- function(st, name = NULL, tb) {
  UseMethod("enrich_dimension_import")
}


#' @rdname enrich_dimension_import
#' @export
enrich_dimension_import.star_schema <-
  function(st, name = NULL, tb) {
    stopifnot(!is.null(name))
    stopifnot(name %in% names(st$dimension))
    length_dimension <- length(st$dimension[[name]][[1]])
    stopifnot(length_dimension > 0)
    enriched_dimension <- dplyr::inner_join(st$dimension[[name]],
                                            tb,
                                            by = intersect(names(st$dimension[[name]]),
                                                           names(tb)))
    stopifnot(length_dimension == length(enriched_dimension[[1]]))
    names_dimension <- names(st$dimension[[name]])
    names_new <- setdiff(names(enriched_dimension), names_dimension)
    st$dimension[[name]] <- enriched_dimension[, c(names_dimension, names_new)]
    if (is_role_playing_dimension(st$dimension[[name]])) {
      for (n in get_role_dimension_names(st, name)) {
        names_role_dimension <- names(st$dimension[[n]])
        for (new in names_new) {
          st$dimension[[n]] <- tibble::add_column(st$dimension[[n]], !!new)
        }
        names(st$dimension[[n]]) <- c(names_role_dimension, names_new)
      }
    }
    st
  }
