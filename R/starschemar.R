#' Obtaining Star Schemas from Flat Tables
#'
#' Transformations that allow obtaining star schemas from flat tables.
#'
#' From flat tables star schemas can be defined that can form constellations
#' (*star schema and constellation definition functions*). Dimensions contain
#' data without duplicates, operations to do data cleaning can be applied on
#' them (*data cleaning functions*). Dimensions can be enriched by adding
#' additional columns, sometimes using functions, others explicitly defined by
#' the user (*dimension enrichment functions*). When new data is obtained, it is
#' necessary to refresh the existing data with them by means of incremental
#' refresh operations or delete data that is no longer necessary (*incremental
#' refresh functions*). Finally, the results obtained can be exported to be
#' consulted with other tools (*results export functions*) or through the
#' defined query functions (*query functions*).
#'
#' @section Star schema and constellation definition: Starting from a flat
#'   table, a dimensional model is defined specifying the attributes that make
#'   up each of the dimensions and the measurements in the facts. The result is
#'   a `dimensional_model` object. It is carried out through the following
#'   *dimensional model definition functions*:
#' - [dimensional_model()]
#' - [define_dimension()]
#' - [define_fact()]
#'
#' A star schema is defined from a flat table and a dimensional model
#' definition. Once defined, a star schema can be transformed by defining role
#' playing dimensions, changing the writing style of element names or the type
#' of dimension attributes. These operations are carried out through the
#' following *star schema definition and transformation functions*:
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
#' *Update definition functions*:
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
#' @section Dimension enrichment: To enrich a dimension with new attributes
#'   related to others already included in it, first, we export the attributes
#'   on which the new ones depend, then we define the new attributes, and import
#'   the table with all the attributes to be added to the dimension.
#' - [enrich_dimension_export()]
#' - [enrich_dimension_import()]
#'
#' @section Incremental refresh: When new data is obtained, an incremental
#'   refresh of the data can be carried out, both of the dimensions and of the
#'   facts. Incremental refresh can be applied to both star schema and
#'   constellation, using the following functions:
#' - [incremental_refresh_star_schema()]
#' - [incremental_refresh_constellation()]
#'
#' Sometimes the data refresh consists of eliminating data that is no longer
#' necessary, generally because it corresponds to a period that has stopped
#' being analyzed but it can also be for other reasons. This data can be
#' selected using the following function:
#' - [filter_fact_rows()]
#'
#' Once the fact data is removed (using the other incremental refresh
#' functions), we can remove the data for the dimensions that are no longer
#' needed using the following function:
#' - [purge_dimensions()]
#'
#' @section Results export: Once the data has been properly structured and
#'   transformed, it can be exported to be consulted with other tools or with R.
#'   Various export formats have been defined, both for star schemas and for
#'   constellations, using the following functions:
#' - [star_schema_as_flat_table()]
#' - [star_schema_as_multistar()]
#' - [star_schema_as_tibble_list()]
#' - [constellation_as_multistar()]
#' - [constellation_as_tibble_list()]
#'
#' @section Query functions: There are many multidimensional query tools
#'   available. The exported data, once stored in files, can be used directly
#'   from them. Using the following functions, you can also perform basic
#'   queries from R on data in the `multistar` format:
#' - [dimensional_query()]
#' - [select_fact()]
#' - [select_dimension()]
#' - [filter_dimension()]
#' - [run_query()]
#'
#' @docType package
#' @name starschemar
NULL


