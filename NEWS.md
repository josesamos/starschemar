
# starschemar 1.1.0
* New dimension enrichment features: [enrich_dimension_export()] and [enrich_dimension_import()].
* New functions to filter facts and purge dimensions (delete unnecessary data): [filter_fact_rows()] and [purge_dimensions()].
* New query definition functions on the exported multistar format: [dimensional_query()], [select_fact()], [select_dimension()], [filter_dimension()] and [run_query()].
* New functions to rename dimensions, attributes, facts and measures in a star schema: [rename_dimension()], [get_dimension_attribute_names()], [rename_dimension_attributes()], [rename_fact()], [get_measure_names()] and [rename_measures()].
* New function to export a multistar (possibly obtained as a result of a query) as a flat table: [multistar_as_flat_table()].
* New data available to develop additional examples: [ft_datagov_uk].

# starschemar 1.0.0
* Initial functionality for defining star schemas and constallations.
* Data cleaning.
* Incremental refresh.
* Results export.
