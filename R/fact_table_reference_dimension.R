
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
    UseMethod("reference_dimension")
  }


#' @rdname reference_dimension
#' @export
#' @keywords internal
reference_dimension.fact_table <-
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
