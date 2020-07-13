

#' Title
#'
#' @param dim
#' @param length_integers
#' @param NA_replacement_value
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
character_dimension <-
  function(dim,
           length_integers = TRUE,
           NA_replacement_value = NULL) {
    UseMethod("character_dimension")
  }


#' @rdname character_dimension
#' @export
#' @keywords internal
character_dimension.dimension_table <-
  function(dim,
           length_integers = TRUE,
           NA_replacement_value = NULL) {
    types <- dplyr::summarise_all(dim[, -1], class)
    for (n in names(dim[, -1])) {
      type <- types[[n]][1]
      if (type != "Date" &
          type != "POSIXct" &
          type != "logical") {
        if (type == "integer" & n %in% names(length_integers)) {
          template <- sprintf("%%0%dd", length_integers[[n]])
          tmp <- sprintf(template, dim[[n]])
          tmp[is.na(dim[[n]])] <- NA
        } else {
          tmp <- as.character(dim[[n]])
        }
        dim[[n]] <- tmp
        if (!is.null(NA_replacement_value)) {
          dim[[n]] <-  tidyr::replace_na(dim[[n]], NA_replacement_value)
        }
      }
    }
    dim
  }
