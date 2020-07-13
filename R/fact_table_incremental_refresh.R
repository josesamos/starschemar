
#' Title
#'
#' @param ft
#' @param ft_new
#' @param existing
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
incremental_refresh_fact <- function(ft, ft_new, existing) {
  UseMethod("incremental_refresh_fact")
}


#' @rdname incremental_refresh_fact
#' @export
#' @keywords internal
incremental_refresh_fact.fact_table <-
  function(ft, ft_new, existing) {
    fk <- attr(ft, "foreign_keys")
    ft_fk <- as.data.frame(ft[, fk])
    ft_new_fk <- as.data.frame(ft_new[, fk])
    exist <- dplyr::intersect(ft_fk, ft_new_fk)
    new <- dplyr::setdiff(ft_new_fk, ft_fk)

    if (nrow(new) > 0) {
      class <- attr(ft, "class")
      if (nrow(exist) == 0) {
        ft <-
          tibble::as_tibble(dplyr::bind_rows(as.data.frame(ft), as.data.frame(ft_new[, names(ft)])))
      } else {
        sel_new <- selection_bit_map(ft_new, new)
        ft <-
          tibble::as_tibble(dplyr::bind_rows(as.data.frame(ft), as.data.frame(ft_new[sel_new, names(ft)])))
      }
      attr(ft, "class") <-  class
    }
    if (nrow(exist) > 0 & existing != "ignore") {
      sel_new <- selection_bit_map(ft_new, exist)
      if (existing == "replace") {
        ft <- replace_records(ft, ft_new[sel_new, names(ft)], fk)
      } else {
        ft <- group_records(ft, ft_new[sel_new, names(ft)], fk)
      }
    }
    ft
  }


#' Title
#'
#' @param t
#' @param values
#'
#' @return
#' @keywords internal
#' @noRd
selection_bit_map <- function(t, values) {
  record <- rep(TRUE, nrow(t))
  nval <- nrow(values)
  for (n in names(values)) {
    record <- record & (t[[n]] %in% unique(values[[n]]))
  }
  record
}

#' Title
#'
#' @param ft
#' @param ft_new
#' @param fk
#'
#' @return
#' @keywords internal
#' @noRd
replace_records <- function(ft, ft_new, fk) {
  for (i in 1:nrow(ft_new)) {
    record <- rep(TRUE, nrow(ft))
    for (n in fk) {
      record <- record & (ft[[n]] == ft_new[[n]][i])
    }
    for (n in names(ft)) {
      ft[[n]][record] <- ft_new[[n]][i]
    }
  }
  ft
}

#' Title
#'
#' @param ft
#' @param ft_new
#' @param fk
#'
#' @return
#' @keywords internal
#' @noRd
group_records <- function(ft, ft_new, fk) {
  measures <- attr(ft, "measures")
  agg_function <- attr(ft, "agg_functions")
  for (i in (1:nrow(ft_new))) {
    record <- rep(TRUE, nrow(ft))
    for (n in fk) {
      record <- record & (ft[[n]] == ft_new[[n]][i])
    }
    for (j in seq_along(measures)) {
      if (agg_function[j] == "MAX") {
        ft[[measures[j]]][record] <- max(ft[[measures[j]]][record], ft_new[[measures[j]]][i] , na.rm = TRUE)
      } else if (agg_function[j] == "MIN") {
        ft[[measures[j]]][record] <- min(ft[[measures[j]]][record], ft_new[[measures[j]]][i] , na.rm = TRUE)
      } else {
        ft[[measures[j]]][record] <- sum(ft[[measures[j]]][record], ft_new[[measures[j]]][i] , na.rm = TRUE)
      }
    }
  }
  ft
}



