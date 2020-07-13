

#' Title
#'
#' @param st A `star_schema` object.
#' @param fl
#' @param dl
#' @param commondim
#'
#' @return
#'
#' @family star schema functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' @export
star_schema_as_star_export <- function(st,
                                fl = NULL,
                                dl = NULL,
                                commondim = NULL) {
  UseMethod("star_schema_as_star_export")
}


#' @rdname star_schema_as_star_export
#' @export
star_schema_as_star_export.star_schema <- function(st,
                                                   fl = NULL,
                                                   dl = NULL,
                                                   commondim = NULL) {
  fl_names <- names(fl)
  fl <- c(fl, list(st$fact[[1]]))
  names(fl) <- c(fl_names, attr(st$fact[[1]], "name"))

  dim <- get_all_dimensions(st)
  dl_names <- names(dl)
  for (d in seq_along(dim)) {
    name_dim <- attr(dim[[d]], "name")
    if (!(name_dim %in% commondim)) {
      dl <- c(dl, list(dim[[d]]))
      dl_names <- c(dl_names, name_dim)
    }
  }
  names(dl) <- dl_names
  new_star_export(fl, dl)
}
