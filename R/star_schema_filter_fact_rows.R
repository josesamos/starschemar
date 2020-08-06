
#' Filter fact rows
#'
#' Filter fact rows based on dimension conditions in a star schema. Dimensions
#' remain unchanged.
#'
#' Filtered rows can be deleted using the `incremental_refresh_star_schema`
#' function.
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#' @param ... Conditions, defined in exactly the same way as in `dplyr::filter`.
#'
#' @return A `star_schema` object.
#'
#' @family incremental refresh functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' st <- st_mrs_age %>%
#'   filter_fact_rows(name = "when", week <= "03") %>%
#'   filter_fact_rows(name = "where", city == "Bridgeport")
#'
#' st2 <- st_mrs_age %>%
#'   incremental_refresh_star_schema(st, existing = "delete")
#'
#' @export
filter_fact_rows <- function(st,
                        name = NULL,
                        ...) {
  UseMethod("filter_fact_rows")
}


#' @rdname filter_fact_rows
#' @export
filter_fact_rows.star_schema <- function(st,
                                    name = NULL,
                                    ...) {
  stopifnot(!is.null(name))
  stopifnot(name %in% get_dimension_names(st))
  dimension <- get_dimension(st, name)
  key <- dplyr::filter(tibble::as_tibble(dimension), ...)[[1]]
  st$fact[[1]] <- st$fact[[1]][st$fact[[1]][[sprintf("%s_key", name)]] %in% key, ]
  st
}
