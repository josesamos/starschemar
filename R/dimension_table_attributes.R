
# is_role_dimension -------------------------------------------------------

#' Title
#'
#' @param dim
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
is_role_dimension <- function(dim) {
  UseMethod("is_role_dimension")
}


#' @rdname is_role_dimension
#' @export
#' @keywords internal
is_role_dimension.dimension_table <- function(dim) {
  ("role" %in% attr(dim, "type"))
}


# is_role_playing_dimension -----------------------------------------------

#' Title
#'
#' @param dim
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
is_role_playing_dimension <- function(dim) {
  UseMethod("is_role_playing_dimension")
}


#' @rdname is_role_playing_dimension
#' @export
#' @keywords internal
is_role_playing_dimension.dimension_table <- function(dim) {
  ("role_playing" %in% attr(dim, "type"))
}

# get_role_playing_dimension_name -----------------------------------------

#' Title
#'
#' @param dim
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
get_role_playing_dimension_name <- function(dim) {
  UseMethod("get_role_playing_dimension_name")
}


#' @rdname get_role_playing_dimension_name
#' @export
#' @keywords internal
get_role_playing_dimension_name.dimension_table <- function(dim) {
  rp_name <- attr(dim, "role_playing")
  if (is.null(rp_name)) {
    rp_name <- ""
  }
  rp_name
}


# set_role_playing_dimension_name -----------------------------------------

#' Title
#'
#' @param dim
#' @param name
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
set_role_playing_dimension_name <- function(dim, name) {
  UseMethod("set_role_playing_dimension_name")
}


#' @rdname set_role_playing_dimension_name
#' @export
#' @keywords internal
set_role_playing_dimension_name.dimension_table <-
  function(dim, name) {
    attr(dim, "role_playing") <- name
    dim
  }

# set_role_playing_dimension_type -----------------------------------------

#' Title
#'
#' @param dim
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
set_role_playing_dimension_type <- function(dim) {
  UseMethod("set_role_playing_dimension_type")
}


#' @rdname set_role_playing_dimension_type
#' @export
#' @keywords internal
set_role_playing_dimension_type.dimension_table <- function(dim) {
  attr(dim, "type") <- "role_playing"
  dim
}

# get_dimension_name ------------------------------------------------------

#' Title
#'
#' @param dim
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
get_dimension_name <- function(dim) {
  UseMethod("get_dimension_name")
}


#' @rdname get_dimension_name
#' @export
#' @keywords internal
get_dimension_name.dimension_table <- function(dim) {
  attr(dim, "name")
}

# set_dimension_name ------------------------------------------------------

#' Title
#'
#' @param dim
#' @param name
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
set_dimension_name <- function(dim, name) {
  UseMethod("set_dimension_name")
}


#' @rdname set_dimension_name
#' @export
#' @keywords internal
set_dimension_name.dimension_table <- function(dim, name) {
  attr(dim, "name") <- name
  dim
}

# get_dimension_type ------------------------------------------------------

#' Title
#'
#' @param dim
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
get_dimension_type <- function(dim) {
  UseMethod("get_dimension_type")
}


#' @rdname get_dimension_type
#' @export
#' @keywords internal
get_dimension_type.dimension_table <- function(dim) {
  attr(dim, "type")
}

# set_dimension_type ------------------------------------------------------

#' Title
#'
#' @param dim
#' @param type
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
set_dimension_type <- function(dim, type) {
  UseMethod("set_dimension_type")
}


#' @rdname set_dimension_type
#' @export
#' @keywords internal
set_dimension_type.dimension_table <- function(dim, type) {
  attr(dim, "type") <- type
  dim
}

# set_dimension_type_conformed --------------------------------------------

#' Title
#'
#' @param dim
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
set_dimension_type_conformed <- function(dim) {
  UseMethod("set_dimension_type_conformed")
}


#' @rdname set_dimension_type_conformed
#' @export
#' @keywords internal
set_dimension_type_conformed.dimension_table <- function(dim) {
  attr(dim, "type") <- unique(append(attr(dim, "type"), "conformed"))
  dim
}

# is_conformed_dimension --------------------------------------------------

#' Title
#'
#' @param dim
#'
#' @return
#'
#' @examples
#'
#' @keywords internal
is_conformed_dimension <- function(dim) {
  UseMethod("is_conformed_dimension")
}


#' @rdname is_conformed_dimension
#' @export
#' @keywords internal
is_conformed_dimension.dimension_table <- function(dim) {
  ("conformed" %in% attr(dim, "type"))
}

