
#' Run query
#'
#' Once we have selected the facts, dimensions and defined the conditions on the
#' instances, we can execute the query to obtain the result.
#'
#' As an option, we can indicate if we do not want to unify the facts in the
#' case of having the same grain.
#'
#' @param dq A `dimensional_query` object.
#' @param unify_by_grain A boolean, unify facts with the same grain.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' ms <- dimensional_query(ms_mrs) |>
#'   select_dimension(name = "where",
#'                    attributes = c("city", "state")) |>
#'   select_dimension(name = "when",
#'                    attributes = c("when_happened_year")) |>
#'   select_fact(
#'     name = "mrs_age",
#'     measures = c("n_deaths"),
#'     agg_functions = c("MAX")
#'   ) |>
#'   select_fact(
#'     name = "mrs_cause",
#'     measures = c("pneumonia_and_influenza_deaths", "other_deaths")
#'   ) |>
#'   filter_dimension(name = "when", when_happened_week <= "03") |>
#'   filter_dimension(name = "where", city == "Boston") |>
#'   run_query()
#'
#' @export
run_query <- function(dq, unify_by_grain = TRUE) {
  UseMethod("run_query")
}



#' @rdname run_query
#' @export
run_query.dimensional_query <- function(dq, unify_by_grain = TRUE) {
  dq <- define_selected_facts(dq)
  dq <- define_selected_dimensions(dq)
  dq <- filter_selected_instances(dq)
  dq <- delete_unused_foreign_keys(dq)
  dq <- remove_duplicate_dimension_rows(dq)
  dq <- group_facts(dq)
  if (unify_by_grain) {
    dq <- unify_facts_by_grain (dq)
  }
  class(dq$output) <- class(dq$input)[1]
  dq$output
}


#' Define selected facts
#'
#' Measure names are stored as the names of the columns with the aggregation
#' functions.
#'
#' @param dq A `dimensional_query` object.
#'
#' @return A `dimensional_query` object.
#'
#' @keywords internal
define_selected_facts <- function(dq) {
  for (name in names(dq$fact)) {
    # measure names are the names of the columns with the aggregation functions
    dq$output$fact[[name]] <-
      dq$input$fact[[name]][, c(attr(dq$input$fact[[name]], "foreign_keys"), names(dq$fact[[name]]))]
    attr(dq$output$fact[[name]], "measures") <- names(dq$fact[[name]])
    attr(dq$output$fact[[name]], "agg_functions") <- dq$fact[[name]]
  }
  dq
}

#' Define selected dimensions
#'
#' Include the selected dimensions and only the selected attributes in them.
#'
#' @param dq A `dimensional_query` object.
#'
#' @return A `dimensional_query` object.
#'
#' @keywords internal
define_selected_dimensions <- function(dq) {
  for (name in names(dq$dimension)) {
    dq$output$dimension[[name]] <- dq$input$dimension[[name]][, dq$dimension[[name]]]
  }
  dq
}

#' Filter selected instances
#'
#' For some dimensions the instances to include have been defined, we have the
#' value of the primary key. They are filtered for both facts and dimensions.
#'
#' @param dq A `dimensional_query` object.
#'
#' @return A `dimensional_query` object.
#'
#' @keywords internal
filter_selected_instances <- function(dq) {
  for (name in names(dq$key)) {
    # filter facts
    for (f in names(dq$output$fact)) {
      key <- sprintf("%s_key", name)
      if (key %in% names(dq$output$fact[[f]])) {
        dq$output$fact[[f]] <-
          dq$output$fact[[f]][dq$output$fact[[f]][[key]] %in% dq$key[[name]], ]
      }
    }
    # filter dimensions
    if (name %in% names(dq$output$dimension)) {
      dq$output$dimension[[name]] <-
        dq$output$dimension[[name]][dq$output$dimension[[name]][[1]] %in% dq$key[[name]], ]
    }
  }
  dq
}

#' Delete unused foreign keys
#'
#' In facts, remove foreign keys from dimensions not included in the result.
#'
#' @param dq A `dimensional_query` object.
#'
#' @return A `dimensional_query` object.
#'
#' @keywords internal
delete_unused_foreign_keys <- function(dq) {
  for (name in names(dq$output$fact)) {
    fk <- attr(dq$output$fact[[name]], "foreign_keys")
    key_dimensions <- sprintf("%s_key", names(dq$dimension))
    col <-
      which(names(dq$output$fact[[name]]) %in% generics::setdiff(fk, key_dimensions))
    if (length(col) > 0) {
      dq$output$fact[[name]] <- dq$output$fact[[name]][,-c(col)]
    }
    attr(dq$output$fact[[name]], "foreign_keys") <-
      generics::intersect(fk, key_dimensions)
  }
  dq
}

#' Remove duplicate dimension rows
#'
#' After selecting only a few columns of the dimensions, there may be rows with
#' duplicate values. We eliminate duplicates and adapt facts to the new
#' dimensions.
#'
#' @param dq A `dimensional_query` object.
#'
#' @return A `dimensional_query` object.
#'
#' @keywords internal
remove_duplicate_dimension_rows <- function(dq) {
  # remove duplicate dimension rows
  for (name in names(dq$dimension)) {
    # remove duplicates and sort
    ft <-
      dplyr::arrange_all(tibble::as_tibble(unique(dq$output$dimension[[name]][, -1])))
    if (nrow(ft) < nrow(dq$output$dimension[[name]])) {
      # add surrogate primary key
      # := variables for parameter names
      # !! expands the expression into a string
      ft <-
        tibble::add_column(ft,!!sprintf("%s_key", name) := 1:nrow(ft), .before = 1)
      for (f in names(dq$output$fact)) {
        key <- sprintf("%s_key", name)
        if (key %in% names(dq$output$fact[[f]])) {
          dq$output$fact[[f]] <-
            dereference_dimension(dq$output$fact[[f]], dq$output$dimension[[name]])
          dq$output$fact[[f]] <-
            reference_dimension(dq$output$fact[[f]], ft, names(ft)[-1])
        }
      }
      class <- class(dq$output$dimension[[name]])
      dq$output$dimension[[name]] <- ft
      class(dq$output$dimension[[name]]) <- class
    }
  }
  dq
}

#' Group facts
#'
#' Once the external keys have been possibly replaced, group the rows of facts.
#'
#' @param dq A `dimensional_query` object.
#'
#' @return A `dimensional_query` object.
#'
#' @keywords internal
group_facts <- function(dq) {
  for (name in names(dq$output$fact)) {
    dq$output$fact[[name]] <- group_table(dq$output$fact[[name]])
  }
  dq
}

#' Unify facts by grain
#'
#' @param dq A `dimensional_query` object.
#'
#' @return A `dimensional_query` object.
#'
#' @keywords internal
unify_facts_by_grain <- function(dq) {
  fact <- NULL
  unified_fact <- NULL
  names_fact <- names(dq$output$fact)
  for (i in seq_along(names_fact)) {
    if (!(names_fact[i] %in% unified_fact)) {
      fact[[names_fact[i]]] <- dq$output$fact[[names_fact[i]]]
      fk_i <- attr(dq$output$fact[[names_fact[i]]], "foreign_keys")
      agg <- list(fact[[names_fact[i]]])
      for (j in seq_along(names_fact)[seq_along(names_fact) > i]) {
        fk_j <- attr(dq$output$fact[[names_fact[j]]], "foreign_keys")
        if (generics::setequal(fk_i, fk_j)) {
          unified_fact <- c(unified_fact, names_fact[j])
          fact2 <- dq$output$fact[[names_fact[j]]][, c(fk_i, attr(dq$output$fact[[names_fact[j]]], "measures"))]

          for (m in attr(fact2, "measures")) {
            m_new <- sprintf("%s_%s", names_fact[j], m)
            names(fact2)[which(names(fact2) == m)] <- m_new
            attr(fact2, "measures")[which(attr(fact2, "measures") == m)] <- m_new
            names(attr(fact2, "agg_functions"))[which(names(attr(fact2, "agg_functions")) == m)] <- m_new
          }

          attr(fact[[names_fact[i]]], "measures") <-
            c(attr(fact[[names_fact[i]]], "measures"), attr(fact2, "measures"))
          attr(fact[[names_fact[i]]], "agg_functions") <-
            c(attr(fact[[names_fact[i]]], "agg_functions"), attr(fact2, "agg_functions"))

          agg <- c(agg, list(fact2))
        }
      }
      if (length(agg) > 1) {
        if (is.null(fk_i)) {
          par_by = character()
        } else {
          par_by = fk_i
        }
        at <- attributes(fact[[names_fact[i]]])
        fact[[names_fact[i]]] <- purrr::reduce(agg, dplyr::inner_join, by = par_by)
        class(fact[[names_fact[i]]]) <- at$class
        attr(fact[[names_fact[i]]], "name") <- at$name
        attr(fact[[names_fact[i]]], "measures") <- at$measures
        attr(fact[[names_fact[i]]], "agg_functions") <- at$agg_functions
        attr(fact[[names_fact[i]]], "nrow_agg") <- at$nrow_agg
      }
    }
  }
  dq$output$fact <- fact
  dq
}
