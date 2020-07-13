#' Title
#'
#' @param tb
#'
#' @return
#' @keywords internal
#' @noRd
#'
prepare_join <- function(tb) {
  # all attributes of type character
  col <- colnames(tb)
  tb <-
    tibble::as_tibble(data.frame(lapply(tb, as.character), stringsAsFactors = FALSE))
  colnames(tb) <- col

  # replace NA with unknown (for join)
  apply(tb[, , drop = FALSE], 2, function(x)
    tidyr::replace_na(x, "___UNKNOWN___"))
}
