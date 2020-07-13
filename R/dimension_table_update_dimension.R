
#' Title
#'
#' @param dim
#' @param updates
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
update_dimension <- function(dim, updates) {
  UseMethod("update_dimension")
}


#' @rdname update_dimension
#' @export
#' @keywords internal
update_dimension.dimension_table <- function(dim, updates) {
  mod_dim <- dim
  name <- get_dimension_name(dim)
  dim_txt <- dim
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


#' Title
#'
#' @param dim
#' @param values
#'
#' @return
#' @keywords internal
#' @noRd
#'
find_values <- function(dim, values) {
  record <- rep(TRUE, nrow(dim))
  for (n in names(values)) {
    record <- record & (dim[, n] == values[n])
  }
  record
}

#' Title
#'
#' @param value
#' @param type
#'
#' @return
#' @keywords internal
#' @noRd
#'
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

