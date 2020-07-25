#' Obtaining Star Schemas from Flat Tables
#'
#' Transformations that allow obtaining star schemas from flat tables.
#'
#' From flat tables star schemas can be defined that can form constellations
#' (*star schema and constellation definition functions*). Dimensions contain
#' data without duplicates, operations to do data cleaning can be applied on
#' them (*data cleaning functions*). When new data is obtained, it is necessary
#' to refresh the existing data with them by means of incremental refresh
#' operations (*incremental refresh functions*). Finally, the results obtained
#' can be exported to be consulted with other tools (*results export
#' functions*).
#'
#' @section Star schema and constellation definition: Starting from a flat
#'   table, a star is defined specifying the attributes that make up each of the
#'   dimensions and the measurements in the facts. The result is a
#'   `star_definition` object. It is carried out through the following *star
#'   definition functions*:
#' - [star_definition()]
#' - [define_dimension()]
#' - [define_fact()]
#'
#' A star schema is defined from a flat table and a star definition. Once
#' defined, a star schema can be transformed by defining role playing
#' dimensions, changing the writing style of element names or the type of
#' dimension attributes. These operations are carried out through the following
#' *star schema definition and transformation functions*:
#' - [star_schema()]
#' - [role_playing_dimension()]
#' - [snake_case()]
#' - [character_dimensions()]
#'
#' Based on various star schemas, a constellation can be defined in which star
#' schemas share common dimensions. Dimensions with the same name must be
#' shared. It is defined by the following *constellation definition function*:
#' - [constellation()]
#'
#' @section Data cleaning: Once the star schemas and constellations are defined,
#'   data cleaning operations can be carried out on dimensions. There are three
#'   groups of functions: one to obtain dimensions of star schemas and
#'   constellations; another to define data cleaning operations over dimensions;
#'   and one more to apply operations to star schemas or constellations.
#'
#' *Obtaining dimensions*:
#' - [get_dimension_names()]
#' - [get_dimension()]
#' - [get_conformed_dimension_names()]
#' - [get_conformed_dimension()]
#'
#' *Operations definition functions*:
#' - [record_update_set()]
#' - [match_records()]
#' - [update_record()]
#' - [update_selection()]
#' - [update_selection_general()]
#'
#' *Modification application functions*:
#' - [modify_dimension_records()]
#' - [modify_conformed_dimension_records()]
#'
#' @section Incremental refresh: When new data is obtained, an incremental
#'   refresh of the data can be carried out, both of the dimensions and of the
#'   facts. Incremental refresh can be applied to both star schema and
#'   constellation, using the following functions:
#' - [incremental_refresh_star_schema()]
#' - [incremental_refresh_constellation()]
#'
#' @section Results export: Once the data has been properly structured and
#'   transformed, it can be exported to be consulted with other tools. Various
#'   export formats have been defined, both for star schemas and for
#'   constellations, using the following functions:
#' - [star_schema_as_flat_table()]
#' - [star_schema_as_multistar()]
#' - [star_schema_as_tibble_list()]
#' - [constellation_as_multistar()]
#' - [constellation_as_tibble_list()]
#'
#' @docType package
#' @name starschemar
NULL

