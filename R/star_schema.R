#' star_schema S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param ft A `tibble`, implements a flat table.
#' @param sd A `dimensional_model` object.
#'
#' @return A `star_schema` object.
#'
#' @importFrom rlang :=
#'
#' @keywords internal
new_star_schema <-
  function(ft = tibble::tibble(),
           sd = dimensional_model()) {
    # Check the type of the base object
    stopifnot(tibble::is_tibble(ft))

    measures_type <-
      dplyr::summarise_all(ft[, sd$fact$measures], class)
    for (n in seq_along(measures_type)) {
      type <- measures_type[[n]][1]
      stopifnot(type %in% c("integer", "double", "integer64", "numeric"))
    }

    star <-
      list(fact = vector("list", length = 1),
           dimension =  vector("list", length = length(sd$dimension)))
    names(star$fact) <- sd$fact$name
    names(star$dimension) <- names(sd$dimension)

    dim_col <- unlist(sd$dimension)
    ft_dim <- ft[, dim_col] # original types for dim.
    ft[, dim_col] <- prepare_join(ft[, dim_col])
    # dim. attributes in fact table will be removed (char and without NA)
    facts <-
      ft[, unlist(c(dim_col, sd$fact$measures), use.names = FALSE)]
    facts <-
      tibble::add_column(facts, !!(sd$fact$nrow_agg) := as.integer(1))
    star$fact[[1]] <-
      new_fact_table(
        facts,
        name = sd$fact$name,
        measures = c(sd$fact$measures, sd$fact$nrow_agg),
        agg_functions = c(sd$fact$agg_functions, "SUM"),
        nrow_agg = sd$fact$nrow_agg
      )
    for (d in rev(seq_along(sd$dimension))) {
      star$dimension[[d]] <-
        new_dimension_table(ft_dim[, sd$dimension[[d]]], names(sd$dimension)[d])

      star$fact[[1]] <-
        reference_dimension(star$fact[[1]], star$dimension[[d]], names(star$dimension[[d]])[-1])
    }

    star$fact[[1]] <- group_table(star$fact[[1]])

    structure(star,
              class = "star_schema")
  }


#' `star_schema` S3 class
#'
#' Creates a `star_schema` object from a flat table (implemented by a `tibble`)
#' and a `dimensional_model` object.
#'
#' Transforms the flat table data according to the facts and dimension
#' definitions of the `dimensional_model` object. Each dimension is generated with
#' a surrogate key which is a foreign key in facts.
#'
#' Facts only contain measurements and foreign keys.
#'
#' @inheritParams new_star_schema
#'
#' @return A `star_schema` object.
#'
#' @family star schema and constellation definition functions
#' @seealso \code{\link{dimensional_model}}
#'
#' @examples
#'
#' st <- star_schema(mrs_age, dm_mrs_age)
#'
#' @export
star_schema <- function(ft, sd) {
  new_star_schema(tibble::as_tibble(ft), sd)
}
