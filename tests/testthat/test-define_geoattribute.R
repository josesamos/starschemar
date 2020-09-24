context("test define_geoattribute")

# skip("geometry in tibble error")
library(sf) # It has to be included even if it is not used directly.

test_that("define_geoattribute works", {
  gms <- geomultistar(ms = ms_mrs_test, geodimension = "where")
  gms <-
    define_geoattribute(
      gms,
      attribute = "city",
      from_layer = usa_cities,
      by = c("city" = "city", "state" = "state")
    )

  expect_equal(
    gms$geodimension$where$city,
    structure(
      list(
        where_key = 1:3,
        city = c("Bridgepor", "Bridgeport",
                 "Tacoma"),
        geometry = structure(
          list(
            structure(c(NA_real_, NA_real_), class = c("XY", "POINT", "sfg")),
            structure(c(-73.2048348,
                        41.1670412), class = c("XY", "POINT", "sfg")),
            structure(c(NA_real_,
                        NA_real_), class = c("XY", "POINT", "sfg"))
          ),
          class = c("sfc_POINT",
                    "sfc"),
          precision = 0,
          bbox = structure(
            c(
              xmin = -73.2048348,
              ymin = 41.1670412,
              xmax = -73.2048348,
              ymax = 41.1670412
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
      row.names = c(NA,-3L),
      name = "where",
      type = "conformed",
      class = c("sf", "tbl_df",
                "tbl", "data.frame", "dimension_table"),
      sf_column = "geometry",
      agr = structure(
        c(where_key = NA_integer_,
          city = NA_integer_),
        class = "factor",
        .Label = c("constant",
                   "aggregate", "identity")
      )
    )
  )

  gms <-
    define_geoattribute(
      gms,
      attribute = "region",
      from_attribute = "city",
    )

  expect_equal(
    gms$geodimension$where$region,
    structure(
      list(
        where_key = 1:3,
        region = c("1", "1", "9"),
        geometry = structure(
          list(
            structure(c(-73.2048348, 41.1670412), class = c("XY", "POINT",
                                                            "sfg")),
            structure(c(-73.2048348, 41.1670412), class = c("XY",
                                                            "POINT", "sfg")),
            structure(c(NA_real_, NA_real_), class = c("XY",
                                                       "POINT", "sfg"))
          ),
          class = c("sfc_POINT", "sfc"),
          precision = 0,
          bbox = structure(
            c(
              xmin = -73.2048348,
              ymin = 41.1670412,
              xmax = -73.2048348,
              ymax = 41.1670412
            ),
            class = "bbox"
          ),
          crs = structure(
            list(input = "NAD83", wkt = "GEOGCRS[\"NAD83\",\n    DATUM[\"North American Datum 1983\",\n        ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"latitude\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"longitude\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4269]]"),
            class = "crs"
          ),
          n_empty = 1L
        )
      ),
      row.names = c(NA,-3L),
      name = "where",
      type = "conformed",
      class = c("sf", "tbl_df",
                "tbl", "data.frame", "dimension_table"),
      sf_column = "geometry",
      agr = structure(
        c(where_key = NA_integer_,
          region = NA_integer_),
        class = "factor",
        .Label = c("constant",
                   "aggregate", "identity")
      )
    )
  )

  gms <-
    define_geoattribute(
      gms,
      attribute = "all_where",
      from_layer = usa_nation,
    )
  expect_equal(class(gms$geodimension$where$all_where$geometry), c("sfc_MULTIPOLYGON", "sfc"))

  gms <-
    define_geoattribute(
      gms,
      attribute = "all_where",
      from_attribute = "region",
    )
  expect_equal(
    gms$geodimension$where$all_where,
    structure(
      list(
        all_where = 0,
        geometry = structure(
          list(structure(
            c(-73.2048348,
              41.1670412), class = c("XY", "POINT", "sfg")
          )),
          class = c("sfc_POINT",
                    "sfc"),
          precision = 0,
          bbox = structure(
            c(
              xmin = -73.2048348,
              ymin = 41.1670412,
              xmax = -73.2048348,
              ymax = 41.1670412
            ),
            class = "bbox"
          ),
          crs = structure(
            list(input = "NAD83", wkt = "GEOGCRS[\"NAD83\",\n    DATUM[\"North American Datum 1983\",\n        ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"latitude\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"longitude\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4269]]"),
            class = "crs"
          ),
          n_empty = 0L
        )
      ),
      row.names = c(NA,-1L),
      class = c("sf", "tbl_df", "tbl", "data.frame"),
      sf_column = "geometry",
      agr = structure(
        c(all_where = NA_integer_),
        class = "factor",
        .Label = c("constant",
                   "aggregate", "identity")
      )
    )
  )
})
