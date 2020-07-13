

#' Title
#'
#' @param ft
#' @param dim
#' @param attributes
#' @param conversion
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
reference_dimension <-
  function(ft, dim, attributes, conversion = TRUE) {
    UseMethod("reference_dimension")
  }


#' @rdname reference_dimension
#' @export
#' @keywords internal
reference_dimension.fact_table <-
  function(ft, dim, attributes, conversion = TRUE) {
    if (conversion) {
      dim[, -1] <- prepare_join(dim[, -1]) # except key
    }
    # union with dimension
    ft <- dplyr::inner_join(ft, dim, by = attributes)
    # remove attributes from dimension
    ft <- ft[,-which(names(ft) %in% attributes)]
    for (i in 1:(length(names(dim)) - length(attributes))) {
      ft <- dplyr::relocate(tibble::as_tibble(ft), tidyr::last_col())
    }
    # restore the object class
    class(ft) <-  unique(append(class(ft), "fact_table"))
    ft
  }
