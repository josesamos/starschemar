
#' Incrementally refresh a constellation with a star schema
#'
#' Incrementally refresh a star schema in a constellation with the content of a
#' new star schema that is integrated into the first.
#'
#' Once the dimensions are integrated, if there are records in the fact table
#' whose keys match the new ones, new ones can be ignored, they can be replaced
#' by new ones, all of them can be grouped using the aggregation functions, or
#' they can be deleted. Therefore, the possible values of the `existing`
#' parameter are: "ignore", "replace", "group" or "delete".
#'
#' @param ct A `constellation` object.
#' @param st A `star_schema` object.
#' @param existing A string, operation to be performed with records in the fact
#'   table whose keys match.
#'
#' @return A `constellation` object.
#'
#' @family incremental refresh functions
#'
#' @examples
#' library(tidyr)
#'
#' ct <- ct_mrs %>%
#'   incremental_refresh_constellation(st_mrs_age_w10, existing = "replace")
#'
#' ct <- ct_mrs %>%
#'   incremental_refresh_constellation(st_mrs_cause_w10, existing = "group")
#'
#' @export
incremental_refresh_constellation <- function(ct, st, existing = "ignore") {
  UseMethod("incremental_refresh_constellation")
}


#' @rdname incremental_refresh_constellation
#' @export
incremental_refresh_constellation.constellation <-
  function(ct, st, existing = "ignore") {
    stopifnot(existing %in% c("ignore", "replace", "group", "delete"))

    st_name <- get_fact_name(st)
    ct$star[[st_name]] <- incremental_refresh_star_schema(ct$star[[st_name]], st, existing)
    conformed_dimensions <-
      get_conformed_dimension_names_st(ct$star[[st_name]])
    for (d in conformed_dimensions) {
      ct$dimension[[d]] <- get_dimension(ct$star[[st_name]], d)
      attr(ct$dimension[[d]], "type") <- "conformed"
      for (n in names(ct$star)) {
        if (n != st_name) {
          ct$star[[n]] <-
            replace_dimension(ct$star[[n]], d, ct$dimension[[d]])
        }
      }
    }
    ct
  }
