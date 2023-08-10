
#' Group the records in the table
#'
#' Group the records in the table using the aggregation functions for the
#' measurements.
#'
#' @param ft A `fact_table` object.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
group_table <- function(ft) {
  UseMethod("group_table")
}


#' @rdname group_table
#' @export
#' @keywords internal
group_table.fact_table <- function(ft) {
  at <- attributes(ft)
  measures <- attr(ft, "measures")
  dim_keys <- setdiff(names(ft), measures)
  ft_group <- dplyr::group_by(as.data.frame(ft), dplyr::across(dplyr::all_of(dim_keys)))
  agg <- list()
  for (i in seq_along(measures)) {
    if (at$agg_functions[i] == "MAX") {
      df <-
        dplyr::summarize_at(ft_group, dplyr::vars(at$measures[i]), max, na.rm = TRUE)
    } else if (at$agg_functions[i] == "MIN") {
      df <-
        dplyr::summarize_at(ft_group, dplyr::vars(at$measures[i]), min, na.rm = TRUE)
    } else {
      df <-
        dplyr::summarize_at(ft_group, dplyr::vars(at$measures[i]), sum, na.rm = TRUE)
    }
    agg <- c(agg, list(df))
  }
  ft <- purrr::reduce(agg, dplyr::inner_join, by = dim_keys)

  new_fact_table(
    tibble::as_tibble(ft),
    name = at$name,
    measures = at$measures,
    agg_functions = at$agg_functions,
    nrow_agg = at$nrow_agg
  )
}
