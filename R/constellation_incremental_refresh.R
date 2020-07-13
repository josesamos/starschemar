
#' Title
#'
#' existing = "ignore"/"replace"/"group"
#'
#' @param ct A `constellation` object.
#' @param st_new
#' @param existing
#'
#' @return
#'
#' @family constellation functions
#' @seealso
#'
#' @examples
#'
#'
#' @export
incremental_refresh_constellation <- function(ct, st_new, existing = "ignore") {
  UseMethod("incremental_refresh_constellation")
}


#' @rdname incremental_refresh_constellation
#' @export
incremental_refresh_constellation.constellation <-
  function(ct, st_new, existing = "ignore") {
    stopifnot(existing %in% c("ignore", "replace", "group"))

    st_name <- fact_name(st_new)
    ct$star[[st_name]] <- incremental_refresh(ct$star[[st_name]], st_new, existing)
    conformed_dimensions <-
      conformed_dimension_names(ct$star[[st_name]])
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
