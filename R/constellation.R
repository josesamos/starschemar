#' constellation S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param lst A list of star_schema objects.
#' @param name A string.
#'
#' @return A constellation object.
#'
new_constellation <-
  function(lst = list(), name = NULL) {

    if (class(lst) == "star_schema") {
      lst <- list(lst)
      names(lst[[1]]) <- names(lst[[1]]$fact)
    } else {
      names <- c()
      for (s in seq_along(lst)) {
        stopifnot(class(lst[[s]]) == "star_schema")
        names <- c(names, names(lst[[s]]$fact))
      }
      names <- unique(names)
      stopifnot(length(lst) == length(names))
      names(lst) <- names
    }

    cnst <-
      list(
        dimension =  vector("list"),
        star = lst
      )
    structure(cnst,
              class = "constellation",
              name = name)
  }



#' constellation S3 class
#'
#' Creates a constellation object from a data frame. Data frame data is converted
#' to tibble type.
#'
#' @inheritParams new_constellation
#'
#' @return A constellation object.
#' @export
#'
#' @examples
#'
constellation <- function(lst, name = NULL) {
  ct <- new_constellation(lst, name)
  conform_all_dimensions(ct)
}
