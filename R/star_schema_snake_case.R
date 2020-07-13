
#' Transform names according to the snake case style
#'
#' Transform fact, dimension, measurement, and attribute names according to the
#' snake case style.
#'
#' This style is suitable if we are going to work with databases.
#'
#' @param st A `star_schema` object.
#'
#' @return A `star_schema` object.
#'
#' @family star schema functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' st <- star_schema(mrs_age_test, sd_mrs_age) %>%
#'   snake_case()
#'
#' st <- star_schema(mrs_age, sd_mrs_age) %>%
#'   role_playing_dimension(
#'     dim_names = c("when", "when_available"),
#'     name = "When Common",
#'     attributes = c("Date", "Week", "Year")
#'   ) %>%
#'   snake_case()
#'
#' @export
snake_case <- function(st) {
  UseMethod("snake_case")
}


#' @rdname snake_case
#' @export
snake_case.star_schema <- function(st) {
  sep = "_"

  names(st$fact) <-
    snakecase::to_snake_case(names(st$fact), sep_out = sep)
  st$fact[[1]] <- snake_case_fact(st$fact[[1]])

  names(st$dimension) <-
    snakecase::to_snake_case(names(st$dimension), sep_out = sep)
  for (d in seq_along(st$dimension)) {
    st$dimension[[d]] <- snake_case_dimension(st$dimension[[d]])
  }
  st
}
