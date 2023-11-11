#' `multistar` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' It only distinguishes between general and conformed dimensions, each
#' dimension has its own data. It can contain multiple fact tables.
#'
#' @param fl A `fact_table` list.
#' @param dl A `dimension_table` list.
#'
#' @return A `multistar` object.
#' @keywords internal
new_multistar <-
  function(fl = list(), dl = list()) {
    star <-
      list(
        fact = vector("list", length = length(fl)),
        dimension =  vector("list", length = length(dl))
      )
    names(star$fact) <- names(fl)
    names(star$dimension) <- names(dl)
    for (f in seq_along(fl)) {
      star$fact[[f]] <- fl[[f]]
      attr(star$fact[[f]], "spec") <- NULL
    }

    for (d in seq_along(dl)) {
      star$dimension[[d]] <- dl[[d]]
      attr(star$dimension[[d]], "role_playing") <- NULL
      attr(star$dimension[[d]], "spec") <- NULL
      if ("conformed" %in% attr(star$dimension[[d]], "type")) {
        attr(star$dimension[[d]], "type") <- "conformed"
      } else {
        attr(star$dimension[[d]], "type") <- "general"
      }
    }

    structure(star,
              class = "multistar")
  }


# prepare_join ------------------------------------------------------------

#' Transform a `tibble` to join
#'
#' Transform all fields in a `tibble` to character type and replace the `NA`
#' with a specific value.
#'
#' @param tb A `tibble`.
#'
#' @return A `tibble`.
#' @keywords internal
prepare_join <- function(tb) {
  n_row <- nrow(tb)
  # all attributes of type character
  col <- colnames(tb)
  tb <- data.frame(lapply(tb, as.character), stringsAsFactors = FALSE)
  colnames(tb) <- col

  # replace NA with unknown (for join)
  tb <- apply(tb[, , drop = FALSE], 2, function(x)
    tidyr::replace_na(x, "___UNKNOWN___"))
  if (n_row == 1) {
    tibble::as_tibble_row(tb)
  } else {
    tibble::as_tibble(tb)
  }
}


#' `fact_table` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param ft A `tibble`, contains the fact table.
#' @param name A string, name of the fact.
#' @param measures A vector of measurement names.
#' @param agg_functions A vector of aggregation function names.
#' @param nrow_agg A string, measurement name for the number of rows aggregated.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
new_fact_table <-
  function(ft = tibble::tibble(),
           name = NULL,
           measures = NULL,
           agg_functions = NULL,
           nrow_agg = NULL) {
    # Check the type of the base object
    stopifnot("Fact table must be a 'tibble'." = tibble::is_tibble(ft))
    stopifnot("The name of facts must be indicated." = !is.null(name))

    fk <- c()
    for (n in names(ft)) {
      if (!(n %in% measures)) {
        fk <- c(fk, n)
      }
    }

    structure(
      ft,
      class = unique(c("fact_table", class(ft))),
      name = name,
      foreign_keys = fk,
      measures = measures,
      agg_functions = agg_functions,
      nrow_agg = nrow_agg
    )
  }


#' Reference a dimension
#'
#' Given a dimension, transform the fact table so that the attributes of the
#' dimension indicated as a parameter, which are in the fact table, are replaced
#' by the other attributes of the dimension.
#'
#' It is used to replace a set of attributes in the fact table with the
#' generated key of the dimension.
#'
#' If necessary, it is also used for the inverse operation: replace the
#' generated key with the rest of attributes (dereference a dimension).
#'
#' @param ft A `fact_table` object.
#' @param dimension A `dimension_table` object.
#' @param attributes A vector of attribute names, attributes used to reference the dimension.
#' @param conversion A boolean, indicates whether the attributes need to be
#'   transformed.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
reference_dimension <-
  function(ft, dimension, attributes, conversion = TRUE) {
    if (conversion) {
      dimension[, -1] <- prepare_join(dimension[, -1]) # except key
    }
    # union with dimension
    ft <- dplyr::inner_join(ft, dimension, by = attributes)
    # remove attributes from dimension
    ft <- ft[,-which(names(ft) %in% attributes)]
    # place rest of them on the left
    for (i in 1:(length(names(dimension)) - length(attributes))) {
      ft <- dplyr::relocate(tibble::as_tibble(ft), tidyr::last_col())
    }
    # restore the object class
    class(ft) <-  unique(c("fact_table", class(ft)))
    ft
  }

#' Dereference a dimension
#'
#' Given a dimension, transform the fact table so that the primary key of the
#' dimension (which is a foreign key in the fact table) is replaced by the other
#' attributes of the dimension.
#'
#' @param ft A `fact_table` object.
#' @param dimension A `dimension_table` object.
#' @param conversion A boolean, indicates whether the attributes need to be
#'   transformed.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
dereference_dimension <-
  function(ft, dimension, conversion = TRUE) {
    reference_dimension(ft, dimension, names(dimension)[1], conversion)
  }


#' Group the records in the table
#'
#' Group the records in the table using the aggregation functions for the
#' measurements.
#'
#' @param ft A `fact_table` object.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
group_table <- function(ft) {
  at <- attributes(ft)
  measures <- attr(ft, "measures")
  dim_keys <- setdiff(names(ft), measures)
  ft_group <- dplyr::group_by(as.data.frame(ft), dplyr::across(dplyr::all_of(dim_keys)))
  agg <- list()
  for (i in seq_along(measures)) {
    if (at$agg_functions[i] == "MAX") {
      df <-
        dplyr::summarize_at(ft_group, dplyr::vars(at$measures[i]), max, na.rm = TRUE)
    } else if (at$agg_functions[i] == "MIN") {
      df <-
        dplyr::summarize_at(ft_group, dplyr::vars(at$measures[i]), min, na.rm = TRUE)
    } else {
      df <-
        dplyr::summarize_at(ft_group, dplyr::vars(at$measures[i]), sum, na.rm = TRUE)
    }
    agg <- c(agg, list(df))
  }
  ft <- purrr::reduce(agg, dplyr::inner_join, by = dim_keys)

  new_fact_table(
    tibble::as_tibble(ft),
    name = at$name,
    measures = at$measures,
    agg_functions = at$agg_functions,
    nrow_agg = at$nrow_agg
  )
}


#' Export a `multistar` as a flat table
#'
#' We can obtain a flat table, implemented using a `tibble`, from a `multistar`
#' (which can be the result of a query). If it only has one fact table, it is
#' not necessary to provide its name.
#'
#' @param ms A `multistar` object.
#' @param fact A string, name of the fact.
#'
#' @return A `tibble`.
#'
#' @family results export functions
#'
#' @examples
#'
#' ft <- ms_mrs |>
#'   multistar_as_flat_table(fact = "mrs_age")
#'
#' ms <- dimensional_query(ms_mrs) |>
#'   select_dimension(name = "where",
#'                    attributes = c("city", "state")) |>
#'   select_dimension(name = "when",
#'                    attributes = c("when_happened_year")) |>
#'   select_fact(name = "mrs_age",
#'               measures = c("n_deaths")) |>
#'   select_fact(
#'     name = "mrs_cause",
#'     measures = c("pneumonia_and_influenza_deaths", "other_deaths")
#'   ) |>
#'   filter_dimension(name = "when", when_happened_week <= "03") |>
#'   filter_dimension(name = "where", city == "Boston") |>
#'   run_query()
#'
#' ft <- ms |>
#'   multistar_as_flat_table()
#'
#' @export
multistar_as_flat_table <- function(ms, fact = NULL) {
  UseMethod("multistar_as_flat_table")
}


#' @rdname multistar_as_flat_table
#' @export
multistar_as_flat_table.multistar <- function(ms, fact = NULL) {
  if (length(ms$fact) == 1) {
    ft <- ms$fact[[1]]
  } else {
    stopifnot("The name of facts must be indicated." = !is.null(fact))
    validate_names(names(ms$fact), fact, concept = 'fact name')
    ft <- ms$fact[[fact]]
  }
  ft_fk <- attr(ft, "foreign_keys")
  for (d in names(ms$dimension)) {
    if (sprintf("%s_key", d) %in% ft_fk) {
      ft <- dereference_dimension(ft, ms$dimension[[d]], conversion = FALSE)
    }
  }
  tibble::as_tibble(ft)
}


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


#' Filter dimension
#'
#' Allows you to define selection conditions for dimension rows.
#'
#' Conditions can be defined on any attribute of the dimension (not only on
#' attributes selected in the query for the dimension). The selection is made
#' based on the function `dplyr::filter`. Conditions are defined in exactly the
#' same way as in that function.
#'
#' @param dq A `dimensional_query` object.
#' @param name A string, name of the dimension.
#' @param ... Conditions, defined in exactly the same way as in `dplyr::filter`.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   filter_dimension(name = "when", when_happened_week <= "03") |>
#'   filter_dimension(name = "where", city == "Boston")
#'
#' @export
filter_dimension <- function(dq,
                             name = NULL,
                             ...) {
  UseMethod("filter_dimension")
}

#' @rdname filter_dimension
#' @export
filter_dimension.dimensional_query <- function(dq,
                                               name = NULL,
                                               ...) {
  stopifnot("The name of the dimension must be indicated." = !is.null(name))
  stopifnot("The name does not correspond to any dimension." = name %in% names(dq$input$dimension))
  stopifnot("The dimension has already been filtered." = !(name %in% names(dq$key)))
  key <- dplyr::filter(tibble::as_tibble(dq$input$dimension[[name]]), ...)[[1]]
  if (is.null(dq$key)) {
    dq$key <- list(name = key)
    names(dq$key) <- name
  } else {
    dim_names <- names(dq$key)
    dq$key <- c(dq$key, list(name = key))
    names(dq$key) <- c(dim_names, name)
  }
  dq
}


#' Select fact
#'
#' To define the fact to be consulted, its name is indicated, optionally, a
#' vector of names of selected measures and another of aggregation functions are
#' also indicated.
#'
#' If the name of any measure is not indicated, only the one corresponding to
#' the number of aggregated rows is included, which is always included.
#'
#' If no aggregation function is included, those defined for the measures are
#' considered.
#'
#' @param dq A `dimensional_query` object.
#' @param name A string, name of the fact.
#' @param measures A vector of measure names.
#' @param agg_functions A vector of aggregation function names. If none is
#'   indicated, those defined in the fact table are considered.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   select_fact(
#'     name = "mrs_age",
#'     measures = c("n_deaths"),
#'     agg_functions = c("MAX")
#'   )
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   select_fact(name = "mrs_age",
#'              measures = c("n_deaths"))
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   select_fact(name = "mrs_age")
#'
#' @export
select_fact <- function(dq,
                        name = NULL,
                        measures = NULL,
                        agg_functions = NULL) {
  UseMethod("select_fact")
}


#' @rdname select_fact
#' @export
select_fact.dimensional_query <- function(dq,
                                          name = NULL,
                                          measures = NULL,
                                          agg_functions = NULL) {
  stopifnot("The name of the fact must be indicated." = !is.null(name))
  validate_names(names(dq$input$fact), name, concept = 'fact name')
  stopifnot("The fact had already been selected." = !(name %in% names(dq$fact)))
  stopifnot("There are repeated measures" = length(measures) == length(unique(measures)))
  for (af in agg_functions) {
    validate_names(c("SUM", "MAX", "MIN"), af, concept = 'aggregation function')
  }
  all_measures <- attr(dq$input$fact[[name]], "measures")
  nrow_agg <- attr(dq$input$fact[[name]], "nrow_agg")
  pos <- which(all_measures == nrow_agg)
  all_measures <- all_measures[-pos]
  all_functions <- attr(dq$input$fact[[name]], "agg_functions")
  all_functions <- all_functions[-pos]
  for (measure in measures) {
    validate_names(all_measures, measure, concept = 'measure')
  }
  if (length(agg_functions) > 0) {
    stopifnot("Measures and aggregation functions do not correspond." = length(measures) == length(agg_functions))
  } else {
    measures <- all_measures[which(measures %in% all_measures)]
    agg_functions <- all_functions[which(measures %in% all_measures)]
  }
  attributes <- c(agg_functions, "SUM")
  names(attributes) <- c(measures, nrow_agg)
  if (is.null(dq$fact)) {
    dq$fact <- list(name = attributes)
    names(dq$fact) <- name
  } else {
    fact_names <- names(dq$fact)
    dq$fact <- c(dq$fact, list(name = attributes))
    names(dq$fact) <- c(fact_names, name)
  }
  dq
}


#' Select dimension
#'
#' To add a dimension in a `dimensional_query` object, we have to define its
#' name and a subset of the dimension attributes. If only the name of the
#' dimension is indicated, it is considered that all its attributes should be
#' added.
#'
#' @param dq A `dimensional_query` object.
#' @param name A string, name of the dimension.
#' @param attributes A vector of attribute names.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   select_dimension(name = "where",
#'                   attributes = c("city", "state")) |>
#'   select_dimension(name = "when")
#'
#' @export
select_dimension <- function(dq,
                             name = NULL,
                             attributes = NULL) {
  UseMethod("select_dimension")
}



#' @rdname select_dimension
#' @export
select_dimension.dimensional_query <- function(dq,
                                               name = NULL,
                                               attributes = NULL) {
  stopifnot("The name of the dimension must be indicated." = !is.null(name))
  validate_names(names(dq$input$dimension), name, concept = 'dimension name')
  stopifnot("The dimension had already been selected." = !(name %in% names(dq$dimension)))
  stopifnot("There are repeated attributes." = length(attributes) == length(unique(attributes)))
  all_attributes <- names(dq$input$dimension[[name]])
  key <- all_attributes[1]
  attributes_defined <- all_attributes[-1]
  for (attribute in attributes) {
    validate_names(attributes_defined, attribute, concept = 'attribute')
  }
  if (is.null(attributes) |
      setequal(attributes, attributes_defined)) {
    attributes <- attributes_defined
  }
  attributes <- c(key, attributes)
  if (is.null(dq$dimension)) {
    dq$dimension <- list(name = attributes)
    names(dq$dimension) <- name
  } else {
    dim_names <- names(dq$dimension)
    dq$dimension <- c(dq$dimension, list(name = attributes))
    names(dq$dimension) <- c(dim_names, name)
  }
  dq
}


#' `dimensional_query` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @return A `dimensional_query` object.
#'
#' @keywords internal
new_dimensional_query <- function(ms = NULL) {
  schema <-
    list(
      fact = NULL,
      dimension = NULL,
      key = NULL,
      input = ms,
      output = NULL
    )

  structure(schema,
            class = "dimensional_query")
}


#' `dimensional_query` S3 class
#'
#' An empty `dimensional_query` object is created where you can select fact
#' measures, dimension attributes and filter dimension rows.
#'
#' @param ms A `multistar` object.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' # ms_mrs <- ct_mrs |>
#' #  constellation_as_multistar()
#'
#' # dq <- dimensional_query(ms_mrs)
#'
#' @export
dimensional_query <- function(ms = NULL) {
  new_dimensional_query(ms)
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

