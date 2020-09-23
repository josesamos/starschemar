#' `geomultistar` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param ms A `multistar` structure.
#' @param geodimension A vector of dimension names.
#'
#' @return A `geomultistar` object.
#'
#' @keywords internal
new_geomultistar <-
  function(ms = NULL, geodimension = NULL) {
    stopifnot(geodimension %in% names(ms$dimension))
    stopifnot(!is.null(geodimension))

    for (dimension in geodimension) {
      if (is.null(ms$geodimension)) {
        ms$geodimension <- list(name = NULL)
        names(ms$geodimension) <- dimension
      } else {
        dim_names <- names(ms$geodimension)
        ms$geodimension <- c(ms$geodimension, list(name = NULL))
        names(ms$geodimension) <- c(dim_names, dimension)
      }
    }

    for (dimension in names(ms$geodimension)) {
      for (attribute in c(sprintf("all_%s", dimension), names(ms$dimension[[dimension]])[-1])) {
        if (is.null(ms$geodimension[[dimension]])) {
          ms$geodimension[[dimension]] <- list(name = NULL)
          names(ms$geodimension[[dimension]]) <- attribute
        } else {
          dim_names <- names(ms$geodimension[[dimension]])
          ms$geodimension[[dimension]] <-
            c(ms$geodimension[[dimension]], list(name = NULL))
          names(ms$geodimension[[dimension]]) <-
            c(dim_names, attribute)
        }
      }
    }

    structure(ms,
              class = c("multistar", "geomultistar"))
  }

#' `geomultistar` S3 class
#'
#' An `geomultistar` object is created. Dimensions that contain geographic
#' information are indicated.
#'
#' @inheritParams new_geomultistar
#'
#' @return A `geomultistar` object.
#'
#' @family geo functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#' library(starschemar)
#'
#' ms_mrs <- ct_mrs %>%
#'   constellation_as_multistar()
#'
#' gms <- geomultistar(ms = ms_mrs, geodimension = "where")
#'
#' @export
geomultistar <- function(ms = NULL, geodimension = NULL) {
  new_geomultistar(ms, geodimension)
}

