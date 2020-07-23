
#' Apply dimension record update operations to a dimension
#'
#' Given a list of dimension record update operations, they are applied on the
#' `dimension_table` object. Update operations must be defined with the set of
#' functions available for that purpose.
#'
#' @param dimension A `dimension_table` object.
#' @param updates A `record_update_set` object.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
update_dimension <- function(dimension, updates) {
  UseMethod("update_dimension")
}


#' @rdname update_dimension
#' @export
#' @keywords internal
update_dimension.dimension_table <- function(dimension, updates) {
  mod_dim <- dimension
  name <- get_dimension_name(dimension)
  dim_txt <- dimension
  dim_txt[, -1] <- prepare_join(dim_txt[, -1])
  for (m in seq_along(updates)) {
    match <- updates[[m]]
    if (match$dimension == name) {
      r_old <- find_values(dim_txt, match$old)
      types <- dplyr::summarise_all(mod_dim, class)
      s <- sum(r_old)
      if (!is.na(s) & s > 0) {
        for (n in names(match$new)) {
          mod_dim[[n]][r_old] <- typed_value(match$new[n], types[[n]][1])
        }
      }
    }
  }
  mod_dim
}


#' Find values in a dimension
#'
#' Find a vector of named values in a dimension.
#'
#' @param dimension A `dimension_table` object.
#' @param values A vector of named values.
#'
#' @return A vector of boolean.
#'
#' @keywords internal
find_values <- function(dimension, values) {
  record <- rep(TRUE, nrow(dimension))
  for (n in names(values)) {
    record <- record & (dimension[, n] == values[n])
  }
  record
}



#' Transform a value according to its type
#'
#' Transform a string value according to its given type.
#'
#' @param value A string.
#' @param type A string
#'
#' @return A typed value.
#'
#' @keywords internal
typed_value <- function(value, type) {
  if (value == "___UNKNOWN___") {
    return(NA)
  } else {
    switch(
      type,
      integer = as.integer(value),
      double = as.double(value),
      logical = as.logical(value),
      complex = as.complex(value),
      raw = as.raw(value),
      factor = as.factor(value),
      ordered = as.ordered(value),
      Date = as.Date(value),
      POSIXct = as.POSIXct(value),
      value
    )
  }
}

