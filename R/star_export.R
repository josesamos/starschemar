#' star_export S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param fl A `fact_table` list.
#' @param dl A `dimension_table` list.
#'
#' @return A `star_export` object.
#' @keywords internal
#' @noRd
#'
new_star_export <-
  function(fl = list(), dl = list) {
    star <-
      list(
        fact = vector("list", length = length(fl)),
        dimension =  vector("list", length = length(dl))
      )
    names(star$fact) <- names(fl)
    names(star$dimension) <- names(dl)
    for (f in seq_along(fl)) {
      star$fact[[f]] <- fl[[f]]
      cl <- class(star$fact[[f]])
      class(star$fact[[f]]) <- cl[cl != "fact_table"]
      attr(star$fact[[f]], "spec") <- NULL
    }

    for (d in seq_along(dl)) {
      star$dimension[[d]] <- dl[[d]]
      attr(star$dimension[[d]], "role_playing") <- NULL
      attr(star$dimension[[d]], "spec") <- NULL
      if ("conformed" %in% attr(star$dimension[[d]], "type")) {
        attr(star$dimension[[d]], "type") <- "conformed"
      } else {
        attr(star$dimension[[d]], "type") <- "general"
      }
      cl <- class(star$dimension[[d]])
      class(star$dimension[[d]]) <- cl[cl != "dimension_table"]
    }

    structure(star,
              class = "star_export")
  }

