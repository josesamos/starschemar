
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
#'
#' @examples
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
  stopifnot("The name of the dimension must be indicated." = !is.null(name))
  validate_names(names(st$dimension), name, concept = 'dimension name')
  stopifnot("There are repeated attributes." = length(attributes) == length(unique(attributes)))
  stopifnot("The dimension must have attributes defined." = length(st$dimension[[name]][[1]]) > 0)
  attributes_defined <- names(st$dimension[[name]])[-1]
  for (attribute in attributes) {
    validate_names(attributes_defined, attribute, concept = 'attribute')
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
#'
#' @examples
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
    stopifnot("The name of the dimension must be indicated." = !is.null(name))
    validate_names(names(st$dimension), name, concept = 'dimension name')
    length_dimension <- length(st$dimension[[name]][[1]])
    stopifnot("The dimension must have attributes defined." = length_dimension > 0)
    enriched_dimension <- dplyr::inner_join(st$dimension[[name]],
                                            tb,
                                            by = intersect(names(st$dimension[[name]]),
                                                           names(tb)))
    stopifnot("The dimension cannot have repeated attributes." = length_dimension == length(unique(enriched_dimension[[1]])))
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


#' Import `tibble` to test to enrich a dimension
#'
#' For a dimension of a star schema a `tibble` is attached. This contains
#' dimension attributes and new attributes. If values associated with all rows
#' in the dimension are included in the `tibble`, the dimension is enriched with
#' the new attributes. This function checks that there are values for all
#' instances. Returns the dimension instances that do not match the imported
#' data.
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#' @param tb A `tibble` object.
#'
#' @return A `dimension` object.
#'
#' @family dimension enrichment functions
#'
#' @examples
#'
#' tb <-
#'   enrich_dimension_export(st_mrs_age,
#'                           name = "when_common",
#'                           attributes = c("week", "year"))
#'
#' # Add new columns with meaningful data (these are not), possibly exporting
#' # data to a file, populating it and importing it.
#' tb <- tibble::add_column(tb, x = "x", y = "y", z = "z")[-1, ]
#'
#' tb2 <- enrich_dimension_import_test(st_mrs_age, name = "when_common", tb)
#'
#' @export
enrich_dimension_import_test <- function(st, name = NULL, tb) {
  UseMethod("enrich_dimension_import_test")
}


#' @rdname enrich_dimension_import_test
#' @export
enrich_dimension_import_test.star_schema <-
  function(st, name = NULL, tb) {
    stopifnot("The name of the dimension must be indicated." = !is.null(name))
    validate_names(names(st$dimension), name, concept = 'dimension name')
    length_dimension <- length(st$dimension[[name]][[1]])
    stopifnot("The dimension must have attributes defined." = length_dimension > 0)
    enriched_dimension <- dplyr::inner_join(st$dimension[[name]],
                                            tb,
                                            by = intersect(names(st$dimension[[name]]),
                                                           names(tb)))
    st$dimension[[name]][!(st$dimension[[name]][[1]] %in% enriched_dimension[[1]]), ]
  }
