

#' Title
#'
#' @param ft
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
snake_case_fact <- function(ft) {
  UseMethod("snake_case_fact")
}


#' @rdname snake_case_fact
#' @export
#' @keywords internal
snake_case_fact.fact_table <- function(ft) {
  sep = "_"
  attr(ft, "name") <-
    snakecase::to_snake_case(attr(ft, "name"), sep_out = sep)
  attr(ft, "foreign_keys") <-
    snakecase::to_snake_case(attr(ft, "foreign_keys"), sep_out = sep)
  attr(ft, "measures") <-
    snakecase::to_snake_case(attr(ft, "measures"), sep_out = sep)
  attr(ft, "nrow_agg") <-
    snakecase::to_snake_case(attr(ft, "nrow_agg"), sep_out = sep)
  names(ft) <- snakecase::to_snake_case(names(ft), sep_out = sep)
  ft
}
