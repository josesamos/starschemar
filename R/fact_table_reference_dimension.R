

#' Title
#'
#' @param ft A `fact_table` object.
#' @param dimension A `dimension_table` object.
#' @param attributes
#' @param conversion
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
    for (i in 1:(length(names(dimension)) - length(attributes))) {
      ft <- dplyr::relocate(tibble::as_tibble(ft), tidyr::last_col())
    }
    # restore the object class
    class(ft) <-  unique(append(class(ft), "fact_table"))
    ft
  }
