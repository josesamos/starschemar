# starschemar 1.2.5
* Update cph information.
* Update the dataset classes.
* Update tests according to the datasets.

# starschemar 1.2.4
* Modify the cph field of the description.
* Include logo on website.
* Divide the available vignette into two parts.

# starschemar 1.2.3
* Define error messages.
* Reorganization of the code to make the `geomultistar` package independent.

# starschemar 1.2.2
* Fix broken @docType package documentation.
* Include notice about the rolap package.

# starschemar 1.2.1
* As a result of queries, when facts with the same granularity are unified, prefix the name of the facts to the name of all the measurements of the facts to unify from the second table (in the previous version, only this prefix was put to the measurement corresponding to the number of records added).
* Fix problems to make it compatible with dplyr 1.1.0.

# starschemar 1.2.0
* New data sets to enrich dimensions: [ft_london_boroughs], [ft_usa_states] and [ft_usa_city_county].
* The data set [ms_mrs] has been defined according to the result obtained in the vignette, so it can be used by other packages.
* New function to get missing instances to enrich a dimension: [enrich_dimension_import_test()].
* New functions to get star schema names and a star schema in a constellation: [get_star_schema_names] and [get_star_schema].
* Improve the structure and content of the vignette.

# starschemar 1.1.0
* New dimension enrichment features: [enrich_dimension_export()] and [enrich_dimension_import()].
* New functions to filter facts and purge dimensions (delete unnecessary data): [filter_fact_rows()] and [purge_dimensions()].
* New query definition functions on the exported multistar format: [dimensional_query()], [select_fact()], [select_dimension()], [filter_dimension()] and [run_query()].
* New functions to rename dimensions, attributes, facts and measures in a star schema: [rename_dimension()], [get_dimension_attribute_names()], [rename_dimension_attributes()], [rename_fact()], [get_measure_names()] and [rename_measures()].
* New function to export a multistar (possibly obtained as a result of a query) as a flat table: [multistar_as_flat_table()].
* New data available to develop additional examples: [ft_datagov_uk].

# starschemar 1.0.0
* Initial functionality for defining star schemas and constellations.
* Data cleaning.
* Incremental refresh.
* Results export.
