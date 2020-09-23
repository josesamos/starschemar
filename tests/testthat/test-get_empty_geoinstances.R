context("test get_empty_geoinstances")

#skip("geometry in tibble error")
library(sf) # It has to be included even if it is not used directly.

test_that("get_empty_geoinstances works", {
  gms <- geomultistar(ms = ms_mrs_test, geodimension = "where")
  gms <-
    define_geoattribute(
      gms,
      attribute = "city",
      from_layer = usa_cities,
      by = c("city" = "city", "state" = "state")
    )

  empty <- get_empty_geoinstances(gms, attribute = "city")

  expect_equal(
    empty,
    structure(
      list(
        where_key = c(1L, 3L),
        city = c("Bridgepor", "Tacoma"),
        geometry = structure(
          list(structure(
            c(NA_real_, NA_real_), class = c("XY",
                                             "POINT", "sfg")
          ), structure(
            c(NA_real_, NA_real_), class = c("XY",
                                             "POINT", "sfg")
          )),
          class = c("sfc_POINT", "sfc"),
          precision = 0,
          bbox = structure(
            c(
              xmin = NA_real_,
              ymin = NA_real_,
              xmax = NA_real_,
              ymax = NA_real_
            ),
            class = "bbox"
          ),
          crs = structure(
            list(input = "NAD83", wkt = "GEOGCRS[\"NAD83\",\n    DATUM[\"North American Datum 1983\",\n        ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"latitude\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"longitude\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4269]]"),
            class = "crs"
          ),
          n_empty = 2L
        )
      ),
      row.names = c(NA,-2L),
      name = "where",
      type = "conformed",
      sf_column = "geometry",
      agr = structure(
        c(where_key = NA_integer_,
          city = NA_integer_),
        .Label = c("constant", "aggregate", "identity"),
        class = "factor"
      ),
      class = c("sf", "tbl_df", "tbl", "data.frame",
                "dimension_table")
    )
  )

})
