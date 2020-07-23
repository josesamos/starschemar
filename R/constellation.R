#' `constellation` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param lst A list of `star_schema` objects.
#' @param name A string.
#'
#' @return A `constellation` object.
#'
#' @keywords internal
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


#' `constellation` S3 class
#'
#' Creates a `constellation` object from a list of `star_schema` objects.
#'
#' @inheritParams new_constellation
#'
#' @return A `constellation` object.
#'
#' @family constellation functions
#' @seealso
#'
#' @examples
#'
#' ct <- constellation(list(st_mrs_age, st_mrs_cause), name = "mrs")
#'
#' @export
constellation <- function(lst, name = NULL) {
  ct <- new_constellation(lst, name)
  conform_all_dimensions(ct)
}


#' Conform all dimensions of a constellation
#'
#' Conform all dimensions with the same name in the star schemas of a
#' constellation. If two dimensions have the same name in a constellation, they
#' must be conformed.
#'
#' @param ct A `constellation` object.
#'
#' @return A `constellation` object.
#'
#' @keywords internal
conform_all_dimensions <- function(ct) {
  names <- c()
  for (s in seq_along(ct$star)) {
    names <- c(names, get_dimension_names(ct$star[[s]]))
  }
  dim_names <- unique(names)
  for (d in dim_names) {
    if (sum(names == d) > 1) {
      ct <- conform_dimensions(ct, name = d)
    }
  }
  ct
}


#' Conform dimensions of given name
#'
#' If two dimensions have the same name in a constellation, they must be
#' conformed.
#'
#' @param ct A `constellation` object.
#' @param name A string, name of the dimension.
#'
#' @return A `constellation` object.
#'
#' @keywords internal
conform_dimensions <- function(ct, name = NULL) {
  dl <- list()
  for (s in seq_along(ct$star)) {
    dim <- get_dimension(ct$star[[s]], name)
    if ("dimension_table" %in% class(dim)) {
      dim <- homogenize(dim)
      dl <- c(dl, list(dim))
    }
  }
  if (length(dl) > 1) {
    d_new <-
      union_of_dimensions(dl, name = name, type = "conformed")
    ct$dimension <- c(ct$dimension, list(d_new))
    names(ct$dimension)[length(ct$dimension)] <- name

    for (s in seq_along(ct$star)) {
      ct$star[[s]] <-
        replace_dimension_in_facts(ct$star[[s]], name, d_new, set_type_conformed = TRUE)
    }
  }
  ct
}

