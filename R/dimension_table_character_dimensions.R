
#' Transform a dimension numeric attributes to character
#'
#' Transforms numeric type attributes of a dimension into character type. It
#' allows indicating the amplitude for some fields, filling with zeros on the
#' left: This is useful to make the alphabetical order of the result correspond
#' to the numerical order. It also allows indicating the literal to be used in
#' case the numerical value is not defined.
#'
#' @param dimension A `dimension_table` object.
#' @param length_integers A `list` of pairs name = lenght, for each attribute name its length.
#' @param NA_replacement_value A string, value to replace NA values.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
character_dimension <-
  function(dimension,
           length_integers = TRUE,
           NA_replacement_value = NULL) {
    UseMethod("character_dimension")
  }


#' @rdname character_dimension
#' @export
#' @keywords internal
character_dimension.dimension_table <-
  function(dimension,
           length_integers = TRUE,
           NA_replacement_value = NULL) {
    types <- dplyr::summarise_all(dimension[,-1], class)
    for (n in names(dimension[,-1])) {
      type <- types[[n]][1]
      if (type != "Date" &
          type != "POSIXct" &
          type != "logical") {
        if (type == "integer" & n %in% names(length_integers)) {
          template <- sprintf("%%0%dd", length_integers[[n]])
          tmp <- sprintf(template, dimension[[n]])
          tmp[is.na(dimension[[n]])] <- NA
        } else {
          tmp <- as.character(dimension[[n]])
        }
        dimension[[n]] <- tmp
        if (!is.null(NA_replacement_value)) {
          dimension[[n]] <-
            tidyr::replace_na(dimension[[n]], NA_replacement_value)
        }
      }
    }
    dimension
  }
