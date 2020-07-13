#' dimension_table S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure. A table with multiple valid relationships between itself and
#' another table is known as a role-playing dimension. This is most commonly
#' seen in dimensions such as Time and Customer.
#' https://www.ibm.com/support/knowledgecenter/en/SSEP7J_11.1.0/com.ibm.swg.ba.cognos.ug_fm.doc/c_bp-multiplerelationships.html
#' A single physical dimension can be referenced multiple times in a fact table,
#' with each reference linking to a logically distinct role for the dimension.
#' For instance, a fact table can have several dates, each of which is
#' represented by a foreign key to the date dimension.  It is essential that
#' each foreign key refers to a separate view of the date dimension so that the
#' references are independent. These separate dimension views (with unique
#' attribute column names) are called roles.
#' https://www.kimballgroup.com/data-warehouse-business-intelligence-resources/kimball-techniques/dimensional-modeling-techniques/role-playing-dimension/
#'
#' dimension types
#' http://dwhlaureate.blogspot.com/2013/08/types-of-dimensions.html
#'
#' type: general, (role , role_playing - shared), (conformed)
#'
#' @param ft A tibble, contains a dimension.
#'
#' @return A dimension_table object.
#'
#' @importFrom rlang :=
#'
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

