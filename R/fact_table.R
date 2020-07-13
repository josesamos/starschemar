#' fact_table S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' type: general, role-playing, conformed, shared (common dimension)
#'
#' @param ft A tibble, contains a dimension.
#'
#' @return A fact_table object.
#'
new_fact_table <-
  function(ft = tibble::tibble(), name = NULL, measures = NULL, agg_functions = NULL, nrow_agg = NULL) {
    # Check the type of the base object
    stopifnot(tibble::is_tibble(ft))
    stopifnot(!is.null(name))

    fk <- c()
    for (n in names(ft)) {
      if (!(n %in% measures)) {
        fk <- c(fk, n)
      }
    }

    structure(
      ft,
      class = unique(append(class(ft), "fact_table")),
      name = name,
      foreign_keys = fk,
      measures = measures,
      agg_functions = agg_functions,
      nrow_agg = nrow_agg
    )
  }

