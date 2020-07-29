
#' Transform dimension numeric attributes to character
#'
#' Transforms numeric type attributes of dimensions into character type. In a
#' `star_schema` numerical data are measurements that are situated in the facts.
#' Numerical data in dimensions are usually codes, day, week, month or year
#' numbers. There are tools that consider any numerical data to be a
#' measurement, for this reason it is appropriate to transform the numerical
#' data of dimensions into character data.
#'
#' It allows indicating the amplitude for some fields, filling with zeros on the
#' left. This is useful to make the alphabetical order of the result correspond
#' to the numerical order.
#'
#' It also allows indicating the literal to be used in case the numerical value
#' is not defined.
#'
#' If a role playing dimension has been defined, the transformation is performed
#' on it.
#'
#' @param st A `star_schema` object.
#' @param length_integers A `list` of pairs name = lenght, for each attribute name its length.
#' @param NA_replacement_value A string, value to replace NA values.
#'
#' @return A `star_schema` object.
#'
#' @family star schema and constellation definition functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' st <- star_schema(mrs_age_test, dm_mrs_age) %>%
#'   role_playing_dimension(
#'     dim_names = c("when", "when_available"),
#'     name = "When Common",
#'     attributes = c("date", "week", "year")
#'   ) %>%
#'   character_dimensions(length_integers = list(week = 2),
#'                        NA_replacement_value = "Unknown")
#'
#' @export
character_dimensions <-
  function(st,
           length_integers = list(),
           NA_replacement_value = NULL) {
    UseMethod("character_dimensions")
  }


#' @rdname character_dimensions
#' @export
character_dimensions.star_schema <-
  function(st,
           length_integers = list(),
           NA_replacement_value = NULL) {
    for (d in seq_along(st$dimension)) {
      if (!is_role_dimension(st$dimension[[d]])) {
        st$dimension[[d]] <-
          character_dimension(st$dimension[[d]],
                        length_integers,
                        NA_replacement_value)
      }
    }
    st
  }
