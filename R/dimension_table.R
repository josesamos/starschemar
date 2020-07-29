#' `dimension_table` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' Types considered: (general), (role, role_playing), (conformed).
#'
#' @param ft A `tibble`, contains a dimension.
#' @param name A string, name of the dimension.
#' @param type A string, type of the dimension.
#'
#' @return A `dimension_table` object.
#'
#' @importFrom rlang :=
#'
#' @keywords internal
new_dimension_table <-
  function(ft = tibble::tibble(), name = NULL, type = "general") {
    # Check the type of the base object
    stopifnot(tibble::is_tibble(ft))
    stopifnot(!is.null(name))

    # remove duplicates and sort
    ft <- dplyr::arrange_all(unique(ft))
    # add surrogate primary key
    # := variables for parameter names
    # !! expands the expression into a string
    ft <- tibble::add_column(ft,!!sprintf("%s_key", name) := 1:nrow(ft), .before = 1)

    structure(
      ft,
      class = unique(append(class(ft), "dimension_table")),
      name = name,
      type = type,
      role_playing = NULL
    )
  }

