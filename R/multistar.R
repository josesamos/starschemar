#' `multistar` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' It only distinguishes between general and conformed dimensions, each
#' dimension has its own data. It can contain multiple fact tables.
#'
#' @param fl A `fact_table` list.
#' @param dl A `dimension_table` list.
#'
#' @return A `multistar` object.
#' @keywords internal
new_multistar <-
  function(fl = list(), dl = list()) {
    star <-
      list(
        fact = vector("list", length = length(fl)),
        dimension =  vector("list", length = length(dl))
      )
    names(star$fact) <- names(fl)
    names(star$dimension) <- names(dl)
    for (f in seq_along(fl)) {
      star$fact[[f]] <- fl[[f]]
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
    }

    structure(star,
              class = "multistar")
  }

