# Get measure names -------------------------------------------------------

#' Get measure names
#'
#' Get the name of measures in facts.
#'
#' @param st A `star_schema` object.
#'
#' @return A vector of measure names.
#'
#' @family rename functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' measure_names <-
#'   st_mrs_age %>% get_measure_names()
#'
#' @export
get_measure_names <- function(st) {
  UseMethod("get_measure_names")
}


#' @rdname get_measure_names
#' @export
get_measure_names.star_schema <- function(st) {
  attr(st$fact[[1]], "measures")
}

# Rename measures -------------------------------------------------------

#' Rename measures
#'
#' Set new names of some measures in facts.
#'
#' @param st A `star_schema` object.
#' @param measures A vector of measure names.
#' @param new_names A vector of new measure names.
#'
#' @return A `star_schema` object.
#'
#' @family rename functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' st <-
#'   st_mrs_age %>% rename_measures(measures = c("deaths"),
#'                                  new_names = c("n_deaths"))
#'
#' @export
rename_measures <- function(st, measures, new_names) {
  UseMethod("rename_measures")
}


#' @rdname rename_measures
#' @export
rename_measures.star_schema <- function(st, measures, new_names) {
  stopifnot(length(measures) == length(unique(new_names)))
  if (attr(st$fact[[1]], "nrow_agg") %in% measures) {
    attr(st$fact[[1]], "nrow_agg") <-
      new_names[which(measures == attr(st$fact[[1]], "nrow_agg"))]
  }
  for (i in seq_along(measures)) {
    stopifnot(measures[i] %in% attr(st$fact[[1]], "measures"))
    attr(st$fact[[1]], "measures")[which(attr(st$fact[[1]], "measures") == measures[i])] <-
      new_names[i]
    names(st$fact[[1]])[which(names(st$fact[[1]]) == measures[i])] <-
      new_names[i]
  }
  st
}

# Get dimension attribute names -------------------------------------------------------

#' Get dimension attribute names
#'
#' Get the name of attributes in a dimension.
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#'
#' @return A vector of attribute names.
#'
#' @family rename functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' attribute_names <-
#'   st_mrs_age %>% get_dimension_attribute_names("when")
#'
#' @export
get_dimension_attribute_names <- function(st, name) {
  UseMethod("get_dimension_attribute_names")
}


#' @rdname get_dimension_attribute_names
#' @export
get_dimension_attribute_names.star_schema <- function(st, name) {
  stopifnot(name %in% names(st$dimension))
  names(st$dimension[[name]])[-1]
}

# Rename dimension attributes -------------------------------------------------------

#' Rename dimension attributes
#'
#' Set new names of some attributes in a dimension.
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#' @param attributes A vector of attribute names.
#' @param new_names A vector of new attribute names.
#'
#' @return A `star_schema` object.
#'
#' @family rename functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' st <-
#'   st_mrs_age %>% rename_dimension_attributes(
#'     name = "when",
#'     attributes = c("week", "year"),
#'     new_names = c("w", "y")
#'   )
#'
#' @export
rename_dimension_attributes <- function(st, name, attributes, new_names) {
  UseMethod("rename_dimension_attributes")
}


#' @rdname rename_dimension_attributes
#' @export
rename_dimension_attributes.star_schema <-
  function(st, name, attributes, new_names) {
    stopifnot(name %in% names(st$dimension))
    stopifnot(length(attributes) == length(unique(new_names)))
    for (i in seq_along(attributes)) {
      stopifnot(attributes[i] %in% names(st$dimension[[name]])[-1])
      names(st$dimension[[name]])[which(names(st$dimension[[name]]) == attributes[i])] <-
        new_names[i]
    }
    st
  }

# Rename fact -------------------------------------------------------

#' Rename fact
#'
#' Set new name for facts.
#'
#' @param st A `star_schema` object.
#' @param name A string, new name of the fact.
#'
#' @return A `star_schema` object.
#'
#' @family rename functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' st <- st_mrs_age %>% rename_fact("age")
#'
#' @export
rename_fact <- function(st, name) {
  UseMethod("rename_fact")
}


#' @rdname rename_fact
#' @export
rename_fact.star_schema <- function(st, name) {
  names(st$fact) <- name
  st$fact[[1]] <- set_fact_name(st$fact[[1]], name)
  st
}


# Rename dimension -------------------------------------------------------

#' Rename dimension
#'
#' Set new name for a dimension.
#'
#' @param st A `star_schema` object.
#' @param name A string, name of the dimension.
#' @param new_name A string, new name of the dimension.
#'
#' @return A `star_schema` object.
#'
#' @family rename functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' st <- st_mrs_age %>%
#'   rename_dimension(name = "when", new_name = "when_happened")
#'
#' @export
rename_dimension <- function(st, name, new_name) {
  UseMethod("rename_dimension")
}


#' @rdname rename_dimension
#' @export
rename_dimension.star_schema <- function(st, name, new_name) {
  stopifnot(name %in% names(st$dimension))
  stopifnot(!(new_name %in% names(st$dimension)))
  names(st$dimension)[which(names(st$dimension) == name)] <-
    new_name
  st$dimension[[new_name]] <-
    set_dimension_name(st$dimension[[new_name]], new_name)
  key <-  sprintf("%s_key", name)
  new_key <-  sprintf("%s_key", new_name)
  names(st$dimension[[new_name]])[which(names(st$dimension[[new_name]]) == key)] <-
    new_key
  if (is_role_playing_dimension(st$dimension[[new_name]])) {
    for (n in get_role_dimension_names(st, name)) {
      st$dimension[[n]] <-
        set_role_playing_dimension_name(st$dimension[[n]], new_name)
    }
  } else {
    names(st$fact[[1]])[which(names(st$fact[[1]]) == key)] <- new_key
    attr(st$fact[[1]], "foreign_keys")[which(attr(st$fact[[1]], "foreign_keys") == key)]  <-
      new_key
  }
  st
}
