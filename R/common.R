
# prepare_join ------------------------------------------------------------

#' Transform a tibble to join
#'
#' Transform all fields in a `tibble` to character type and replace the `NA`
#' with a specific value.
#'
#' @param tb A `tibble`.
#'
#' @return A `tibble`.
#' @keywords internal
prepare_join <- function(tb) {
  # all attributes of type character
  col <- colnames(tb)
  tb <- data.frame(lapply(tb, as.character), stringsAsFactors = FALSE)
  colnames(tb) <- col

  # replace NA with unknown (for join)
  tibble::as_tibble(apply(tb[, , drop = FALSE], 2, function(x)
    tidyr::replace_na(x, "___UNKNOWN___")))
}


# union_of_dimensions -----------------------------------------------------

#' Perform union of dimensions
#'
#' Generates a new dimension from the instances of the dimensions in a list, as
#' the union of the dimensions.
#'
#' @param dimensions List of `dimension_table` objects.
#' @param name A string, name of the dimension.
#' @param type A string, type of the dimension.
#'
#' @return A `dimension_table` object.
#' @keywords internal
union_of_dimensions <- function(dimensions,
                                name = NULL,
                                type = "role_playing") {
  d_new = dimensions[1]
  common_structure <- unlist(dplyr::summarise_all(dimensions[[1]], class)) #
  if (length(dimensions) > 1) {
    dim_union <- as.data.frame(dimensions[1])
    for (i in 2:length(dimensions)) {
      dimension_structure <- unlist(dplyr::summarise_all(dimensions[[i]], class)) #
      stopifnot(dimension_structure == common_structure) #
      dim_union <- dplyr::union_all(dim_union, as.data.frame(dimensions[[i]]))
    }
    dim_union <- tibble::as_tibble(dim_union)
    d_new <-
      new_dimension_table(dim_union, name = name, type = type)
  }
  d_new
}


# update_dimensions -------------------------------------------------------

#' Apply update operations to dimensions
#'
#' Apply dimension record update operations to the dimensions in the list.
#' Returns the list of modified dimensions.
#'
#' @param dimensions List of `dimension_table` objects to update.
#' @param updates A `record_update_set` object.
#'
#' @return List of updated `dimension_table` objects.
#'
#' @keywords internal
update_dimensions <- function(dimensions, updates) {
  mod_dim <- list()
  for (d in seq_along(dimensions)) {
    name <- get_dimension_name(dimensions[[d]])
    if (is_dimension_in_updates(updates, name)) {
      dim <- update_dimension(dimensions[[d]], updates)
      mod_dim <- c(mod_dim, list(dim))
      names(mod_dim)[length(mod_dim)] <- name
    }
  }
  mod_dim
}
