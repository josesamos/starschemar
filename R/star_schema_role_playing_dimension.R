
#' Define a role playing dimension in a `star_schema` object
#'
#' Given a list of `star_schema` dimension names, all with the same structure, a
#' role playing dimension with the indicated name and attributes is generated.
#' The original dimensions become role dimensions defined from the new role
#' playing dimension.
#'
#' After definition, all role dimensions have the same virtual instances (those
#' of the role playing dimension). The foreign keys in facts are adapted to this
#' new situation.
#'
#' @param st A `star_schema` object.
#' @param dim_names A vector of dimension names.
#' @param name A string, name of the role playing dimension.
#' @param attributes A vector of attribute names of the role playing dimension.
#'
#' @return A `star_schema` object.
#'
#' @family star schema functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' st <- star_schema(mrs_age, sd_mrs_age) %>%
#'   role_playing_dimension(
#'     dim_names = c("when", "when_available"),
#'     name = "When Common",
#'     attributes = c("Date", "Week", "Year")
#'   )
#'
#' st <- star_schema(mrs_cause, sd_mrs_cause) %>%
#'   role_playing_dimension(
#'     dim_names = c("when", "when_received", "when_available"),
#'     name = "when_common",
#'     attributes = c("date", "week", "year")
#'   )
#'
#' @export
role_playing_dimension <-
  function(st,
           dim_names,
           name = NULL,
           attributes = NULL) {
    UseMethod("role_playing_dimension")
  }


#' @rdname role_playing_dimension
#' @export
role_playing_dimension.star_schema <-
  function(st,
           dim_names,
           name = NULL,
           attributes = NULL) {

    dl <- list()
    for (d_name in dim_names) {
      dim <- get_dimension(st, d_name)
      st$fact[[1]] <-
        dereference_dimension(st$fact[[1]], dim)
      dim <- homogenize(dim, attributes)
      dl <- c(dl, list(dim))
    }

    d_new <- union_of_dimensions(dl, name = name, type = "role_playing")
    st$dimension <- c(st$dimension, list(d_new))
    names(st$dimension)[length(st$dimension)] <- name

    for (d_name in dim_names) {
      st$dimension[[d_name]] <-
        role_dimension(st$dimension[[d_name]], name)
      dim <- get_dimension(st, d_name)
      st$fact[[1]] <-
        reference_dimension(st$fact[[1]], dim, names(dim)[-1])
    }
    st
  }


#' Perform union of dimensions
#'
#' Generates a new dimension from the instances of the dimensions in a list, as
#' the union of the dimensions.
#'
#' @param dl List of `dimension_table` objects.
#' @param name A string, name of the dimension.
#' @param type A string, type of the dimension.
#'
#' @return A `dimension_table` object.
#' @keywords internal
#' @noRd
#'
union_of_dimensions <-
  function(dl = list(),
           name = NULL,
           type = "role_playing") {
    d_new = dl[1]
    if (length(dl) > 1) {
      dim_union <- as.data.frame(dl[1])
      for (i in 2:length(dl)) {
        dim_union <- dplyr::union_all(dim_union, as.data.frame(dl[[i]]))
      }
      dim_union <- tibble::as_tibble(dim_union)
      d_new <-
        new_dimension_table(dim_union, name = name, type = type)
    }
    d_new
  }
