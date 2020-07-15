
# is_role_dimension -------------------------------------------------------

#' Is it role dimension?
#'
#' Indicates by means of a boolean if the dimension is a role dimension.
#'
#' Attributes can be accessed directly but this function has been defined
#' because it is used from other classes and is thus done in a more controlled
#' way.
#'
#' @param dimension A `dimension_table` object.
#'
#' @return A boolean.
#'
#' @keywords internal
is_role_dimension <- function(dimension) {
  UseMethod("is_role_dimension")
}


#' @rdname is_role_dimension
#' @export
#' @keywords internal
is_role_dimension.dimension_table <- function(dimension) {
  ("role" %in% attr(dimension, "type"))
}


# is_role_playing_dimension -----------------------------------------------

#' Is it role-playing dimension?
#'
#' Indicates by means of a boolean if the dimension is a role-playing dimension.
#'
#' Attributes can be accessed directly but this function has been defined
#' because it is used from other classes and is thus done in a more controlled
#' way.
#'
#' @param dimension A `dimension_table` object.
#'
#' @return A boolean.
#'
#' @keywords internal
is_role_playing_dimension <- function(dimension) {
  UseMethod("is_role_playing_dimension")
}


#' @rdname is_role_playing_dimension
#' @export
#' @keywords internal
is_role_playing_dimension.dimension_table <- function(dimension) {
  ("role_playing" %in% attr(dimension, "type"))
}

# is_conformed_dimension --------------------------------------------------

#' Is it conformed dimension?
#'
#' Indicates by means of a boolean if the dimension is a conformed dimension.
#'
#' Attributes can be accessed directly but this function has been defined
#' because it is used from other classes and is thus done in a more controlled
#' way.
#'
#' @param dimension A `dimension_table` object.
#'
#' @return A boolean.
#'
#' @keywords internal
is_conformed_dimension <- function(dimension) {
  UseMethod("is_conformed_dimension")
}


#' @rdname is_conformed_dimension
#' @export
#' @keywords internal
is_conformed_dimension.dimension_table <- function(dimension) {
  ("conformed" %in% attr(dimension, "type"))
}

# get_role_playing_dimension_name -----------------------------------------

#' Get the associated role-playing dimension name
#'
#' Each role dimension has the name of the role-playing dimension associated.
#' This function allows us to obtain its name.
#'
#' Attributes can be accessed directly but this function has been defined
#' because it is used from other classes and is thus done in a more controlled
#' way.
#'
#' @param dimension A `dimension_table` object.
#'
#' @return A string, name of the dimension.
#'
#' @keywords internal
get_role_playing_dimension_name <- function(dimension) {
  UseMethod("get_role_playing_dimension_name")
}


#' @rdname get_role_playing_dimension_name
#' @export
#' @keywords internal
get_role_playing_dimension_name.dimension_table <- function(dimension) {
  rp_name <- attr(dimension, "role_playing")
  if (is.null(rp_name)) {
    rp_name <- ""
  }
  rp_name
}


# set_role_playing_dimension_name -----------------------------------------

#' Set the associated role-playing dimension name
#'
#' Each role dimension has the name of the role-playing dimension associated.
#' This function allows us to set its name.
#'
#' Attributes can be accessed directly but this function has been defined
#' because it is used from other classes and is thus done in a more controlled
#' way.
#'
#' @param dimension A `dimension_table` object.
#' @param name A string, name of role-playing dimension.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
set_role_playing_dimension_name <- function(dimension, name) {
  UseMethod("set_role_playing_dimension_name")
}


#' @rdname set_role_playing_dimension_name
#' @export
#' @keywords internal
set_role_playing_dimension_name.dimension_table <-
  function(dimension, name) {
    attr(dimension, "role_playing") <- name
    dimension
  }


# get_dimension_name ------------------------------------------------------

#' Get the dimension name
#'
#' Returns the name of the dimension.
#'
#' Attributes can be accessed directly but this function has been defined
#' because it is used from other classes and is thus done in a more controlled
#' way.
#'
#' @param dimension A `dimension_table` object.
#'
#' @return A string, name of the dimension.
#'
#' @keywords internal
get_dimension_name <- function(dimension) {
  UseMethod("get_dimension_name")
}


#' @rdname get_dimension_name
#' @export
#' @keywords internal
get_dimension_name.dimension_table <- function(dimension) {
  attr(dimension, "name")
}

# set_dimension_name ------------------------------------------------------

#' Set the dimension name
#'
#' It allows us to define the name of the dimension.
#'
#' Attributes can be accessed directly but this function has been defined
#' because it is used from other classes and is thus done in a more controlled
#' way.
#'
#' @param dimension A `dimension_table` object.
#' @param name A string, name of the dimension.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
set_dimension_name <- function(dimension, name) {
  UseMethod("set_dimension_name")
}


#' @rdname set_dimension_name
#' @export
#' @keywords internal
set_dimension_name.dimension_table <- function(dimension, name) {
  attr(dimension, "name") <- name
  dimension
}

# get_dimension_type ------------------------------------------------------

#' Get the dimension type
#'
#' Returns the type of the dimension.
#'
#' Attributes can be accessed directly but this function has been defined
#' because it is used from other classes and is thus done in a more controlled
#' way.
#'
#' @param dimension A `dimension_table` object.
#'
#' @return A string, type of the dimension.
#'
#' @keywords internal
get_dimension_type <- function(dimension) {
  UseMethod("get_dimension_type")
}


#' @rdname get_dimension_type
#' @export
#' @keywords internal
get_dimension_type.dimension_table <- function(dimension) {
  attr(dimension, "type")
}

# set_dimension_type ------------------------------------------------------

#' Set the dimension type
#'
#' It allows us to define the type of the dimension.
#'
#' Attributes can be accessed directly but this function has been defined
#' because it is used from other classes and is thus done in a more controlled
#' way.
#'
#' @param dimension A `dimension_table` object.
#' @param type A string, type of the dimension.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
set_dimension_type <- function(dimension, type) {
  UseMethod("set_dimension_type")
}


#' @rdname set_dimension_type
#' @export
#' @keywords internal
set_dimension_type.dimension_table <- function(dimension, type) {
  attr(dimension, "type") <- type
  dimension
}

# set_dimension_type_role_playing -----------------------------------------

#' Set the type of a role-playing dimension
#'
#' It allows us to define the type of a role-playing dimension.
#'
#' Attributes can be accessed directly but this function has been defined
#' because it is used from other classes and is thus done in a more controlled
#' way.
#'
#' @param dimension A `dimension_table` object.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
set_dimension_type_role_playing <- function(dimension) {
  UseMethod("set_dimension_type_role_playing")
}


#' @rdname set_dimension_type_role_playing
#' @export
#' @keywords internal
set_dimension_type_role_playing.dimension_table <- function(dimension) {
  set_dimension_type(dimension, "role_playing")
}

# set_dimension_type_conformed --------------------------------------------

#' Set the type of a conformed dimension
#'
#' It allows us to define the type of a conformed dimension.
#'
#' Attributes can be accessed directly but this function has been defined
#' because it is used from other classes and is thus done in a more controlled
#' way.
#'
#' @param dimension A `dimension_table` object.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
set_dimension_type_conformed <- function(dimension) {
  UseMethod("set_dimension_type_conformed")
}


#' @rdname set_dimension_type_conformed
#' @export
#' @keywords internal
set_dimension_type_conformed.dimension_table <- function(dimension) {
  attr(dimension, "type") <- unique(append(attr(dimension, "type"), "conformed"))
  dimension
}


