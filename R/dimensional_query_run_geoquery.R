
#' Get a geographic vector from a query
#'
#' After defining a query and geographic dimensions, run the query and select
#' the geographic data associated with it to get a geographic data layer as the
#' result.
#'
#' As an option, we can indicate if we do not want to unify the facts in the
#' case of having the same grain.
#'
#' If the result only has one fact table, it is not necessary to provide its
#' name. Nor is it necessary to indicate the name of the geographic dimension if
#' there is only one available. If no attribute is specified, the one
#' corresponding to the *all* level is considered.
#'
#' @param dq A `dimensional_query` object.
#' @param unify_by_grain A boolean, unify facts with the same grain.
#' @param fact A string, name of the fact.
#' @param dimension A string, name of the geographic dimension.
#' @param attribute A string, name of the geographic attribute to consider.
#'
#' @return A `sf` object.
#'
#' @family geo functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#' library(sf) # It has to be included even if it is not used directly.
#'
#' gms <- geomultistar(ms = ms_mrs, geodimension = "where") %>%
#'   define_geoattribute(
#'     attribute = "city",
#'     from_layer = usa_cities,
#'     by = c("city" = "city", "state" = "state")
#'   )  %>%
#'   define_geoattribute(
#'     attribute = "state",
#'     from_layer = usa_states,
#'     by = c("state" = "state")
#'   ) %>%
#'   define_geoattribute(attribute = "region",
#'                       from_attribute = "state") %>%
#'   define_geoattribute(attribute = "all_where",
#'                       from_layer = usa_nation)
#'
#' gdq <- dimensional_query(gms) %>%
#'   select_dimension(name = "where",
#'                    attributes = c("state", "city")) %>%
#'   select_dimension(name = "when",
#'                    attributes = c("year", "week")) %>%
#'   select_fact(
#'     name = "mrs_age",
#'     measures = c("deaths")
#'   ) %>%
#'   select_fact(name = "mrs_cause",
#'               measures = c("pneumonia_and_influenza_deaths", "other_deaths")) %>%
#'   filter_dimension(name = "when", week <= "03") %>%
#'   filter_dimension(name = "where", state == "MA")
#'
#' sf <- gdq %>%
#'   run_geoquery(attribute = "city")
#'
#' @export
run_geoquery <-
  function(dq,
           unify_by_grain = TRUE,
           fact = NULL,
           dimension = NULL,
           attribute = NULL) {
    UseMethod("run_geoquery")
  }

#' @rdname run_geoquery
#' @export
run_geoquery.dimensional_query <-
  function(dq,
           unify_by_grain = TRUE,
           fact = NULL,
           dimension = NULL,
           attribute = NULL) {

    # run_query
    dq <- define_selected_facts(dq)
    dq <- define_selected_dimensions(dq)
    dq <- filter_selected_instances(dq)
    dq <- delete_unused_foreign_keys(dq)
    ###########

    dq <- filter_geodimension(dq)

    if (is.null(dimension)) {
      dimension <- names(dq$output$geodimension)[1]
    }
    all <- sprintf("all_%s", dimension)
    if (is.null(attribute)) {
      attribute <- all
    }
    stopifnot(attribute %in% names(dq$output$geodimension[[dimension]]))

    if (is.null(dq$output$dimension[[dimension]])) {
      # all, geographic dimension not selected in query
      stopifnot(attribute == all)
      geodim <- dq$output$geodimension[[dimension]][[attribute]]
    }
    else {
      columns <- names(dq$output$dimension[[dimension]])
      if (attribute == all) {
        dq$output$dimension[[dimension]][[attribute]] <- 0
      }
      key <-
        names(dq$output$geodimension[[dimension]][[attribute]])[1]
      geodim <-
        dplyr::left_join(dq$output$dimension[[dimension]], dq$output$geodimension[[dimension]][[attribute]][, key], by = key) %>%
        sf::st_as_sf() %>%
        dplyr::select(tidyselect::all_of(columns))
      if (attribute == all) {
        dq$output$dimension[[dimension]] <-
          dq$output$dimension[[dimension]][, 1:(length(names(dq$output$dimension[[dimension]])) - 1)]
      }
    }
    dq$output$geodimension <- NULL

    # run_query
    dq <- remove_duplicate_dimension_rows(dq)
    dq <- group_facts(dq)
    if (unify_by_grain) {
      dq <- unify_facts_by_grain (dq)
    }
    class(dq$output) <- class(dq$input)
    ###########

    ft <- multistar_as_flat_table(dq$output, fact)
    columns <- names(ft)

    if (length(names(geodim)) == 2) {
      # all, geographic dimension not selected in query
      ft[[attribute]] <- 0
      ft <- dplyr::left_join(ft, geodim, by = attribute)
    } else {
      ft <-
        dplyr::left_join(ft, geodim, by = names(geodim)[2:(length(names(geodim)) - 1)])
    }

    ft <- ft %>%
      sf::st_as_sf() %>%
      dplyr::select(tidyselect::all_of(columns)) %>%
      dplyr::group_by_at(columns) %>%
      dplyr::summarize(.groups = "drop")
    ft
  }



#' Filter geodimension
#'
#' Filter the geodimension instances according to the query definition.
#'
#' @param dq A `dimensional_query` object.
#'
#' @return A `dimensional_query` object.
#'
#' @keywords internal
filter_geodimension <- function(dq) {
  for (dimension in names(dq$input$geodimension)) {
    all <- sprintf("all_%s", dimension)
    dq$output$geodimension[[dimension]][[all]] <-
      dq$input$geodimension[[dimension]][[all]]
  }
  sel_geodimensions <-
    intersect(names(dq$dimension), names(dq$input$geodimension))
  for (dimension in sel_geodimensions) {
    for (attribute in dq$dimension[[dimension]][-1]) {
      if (!is.null(dq$input$geodimension[[dimension]][[attribute]])) {
        if (!is.null(dq$key[[dimension]])) {
          sel <- dq$input$geodimension[[dimension]][[attribute]][[1]] %in% dq$key[[dimension]]
          dq$output$geodimension[[dimension]][[attribute]] <-
            dq$input$geodimension[[dimension]][[attribute]][sel, ]
        } else {
          dq$output$geodimension[[dimension]][[attribute]] <-
            dq$input$geodimension[[dimension]][[attribute]]
        }
      }
    }
  }
  dq
}

