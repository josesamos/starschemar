
#' Incrementally refresh a fact table with another
#'
#' Incrementally refresh a fact table with the content of a new one that is
#' integrated into the first.
#'
#' If there are records whose keys match the new ones, we can ignore, replace or
#' group them.
#'
#' @param ft A `fact_table` object.
#' @param ft_new A `fact_table` object, possibly with new data.
#' @param existing A string, operation to be performed with records whose keys
#'   match.
#'
#' @return A `fact_table` object.
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
    sel_exist <- selection_bit_map(ft_new_fk, exist, names(exist))
    sel_new <- !sel_exist
    class <- attr(ft, "class")

    if (nrow(new) > 0) {
      ft <-
        tibble::as_tibble(dplyr::bind_rows(as.data.frame(ft), as.data.frame(ft_new[sel_new, names(ft)])))
    }
    if (nrow(exist) > 0 & existing != "ignore") {
      if (existing == "replace") {
        ft <- replace_records(ft, ft_new[sel_exist, names(ft)], fk)
      } else if (existing == "group") {
        ft <- group_records(ft, ft_new[sel_exist, names(ft)], fk)
      } else if (existing == "delete") {
        ft <- delete_records(ft, ft_new[sel_exist, names(ft)], fk)
      }
    }
    attr(ft, "class") <-  class
    ft
  }


#' Generate a record selection bitmap
#'
#' Obtain a vector of booleans to select the records in the table that have the
#' combination of values.
#'
#' @param table A `tibble`, table to select.
#' @param values A `tibble`, set of values to search.
#' @param names A vector of column names to consider.
#'
#' @return A vector of boolean.
#' @keywords internal
selection_bit_map <- function(table, values, names) {
  res <- rep(FALSE, nrow(table))
  for (i in seq_along(values[[1]])) {
    record <- rep(TRUE, nrow(table))
    for (n in names) {
      record <- record & (table[[n]] == values[[n]][i])
    }
    res <- res | record
  }
  res
}

#' Replace records
#'
#' Replace records with the same primary key.
#'
#' @param ft A `fact_table` object.
#' @param ft_new A `fact_table` object.
#' @param fk A vector of foreign key names.
#'
#' @return A `fact_table` object.
#' @keywords internal
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

#' Group records
#'
#' Group records with the same primary key.
#'
#' @param ft A `fact_table` object.
#' @param ft_new A `fact_table` object.
#' @param fk A vector of foreign key names.
#'
#' @return A `fact_table` object.
#' @keywords internal
group_records <- function(ft, ft_new, fk) {
  measures <- attr(ft, "measures")
  agg_function <- attr(ft, "agg_functions")
  for (i in 1:nrow(ft_new)) {
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

#' Delete records
#'
#' Delete records with the same primary key.
#'
#' @param ft A `fact_table` object.
#' @param ft_new A `fact_table` object.
#' @param fk A vector of foreign key names.
#'
#' @return A `fact_table` object.
#' @keywords internal
delete_records <- function(ft, ft_new, fk) {
  res <- selection_bit_map(ft, ft_new, fk)
  ft[(!res), ]
}



