
<!-- README.md is generated from README.Rmd. Please edit that file -->

# starschemar

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/josesamos/starschemar.svg?branch=master)](https://travis-ci.com/josesamos/starschemar)
<!-- badges: end -->

The *multidimensional data model* was defined in the 1990s with the aim
of supporting data analysis. Data in multidimensional systems is
obtained from operational systems and is transformed to adapt it to the
new structure.

Transformations can be carried out using professional ETL tools.
Recently, tools aimed at end users have emerged, which are also aimed at
performing transformation operations. All these tools are very useful to
carry out the transformation process, they provide a development
environment to define the transformation operations in a general way.

Frequently, the operations to be performed aim to transform a flat table
(with data that comes from operational systems) into a star schema
(which implements a multidimensional system). With the tools mentioned
above, this transformation can be carried out, but it requires a lot of
work. I am not aware of any tools with operations designed to
specifically support this transformation process.

The goal of `starschemar` is to define transformations that allow you to
easily obtain star schemas from flat tables. In addition, it includes
basic data cleaning operations and incremental data refresh operations,
adapted to this context.

## Installation

You can install the released version of `starschemar` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("starschemar")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("josesamos/starschemar")
```

## Example

To illustrate how the package works we will use a small part of the
[Deaths in 122 U.S. cities - 1962-2016. 122 Cities Mortality Reporting
System](https://catalog.data.gov/dataset/deaths-in-122-u-s-cities-1962-2016-122-cities-mortality-reporting-system)
data set in the form of a flat table, the first rows of which are shown
below.

| Year | WEEK | Week Ending Date | REGION | State |    City    | Pneumonia and Influenza Deaths | All Deaths | Other Deaths |
| :--: | :--: | :--------------: | :----: | :---: | :--------: | :----------------------------: | :--------: | :----------: |
| 1962 |  1   |    1962-01-06    |   1    |  CT   | Bridgeport |               3                |     46     |      43      |
| 1962 |  2   |    1962-01-13    |   1    |  CT   | Bridgeport |               2                |     43     |      41      |
| 1962 |  3   |    1962-01-20    |   1    |  CT   | Bridgepor  |               2                |     40     |      38      |
| 1962 |  1   |    1962-01-06    |   9    |  WA   |   Tacoma   |               4                |     50     |      46      |
|  NA  |  NA  |    1962-01-13    |   9    |  WA   |   Tacoma   |               2                |     45     |      43      |
| 1962 |  3   |    1962-01-20    |   9    |  WA   |   Tacoma   |               0                |     39     |      39      |

The original data has been modified to have some erroneous or missing
data.

The transformation to obtain a star schema from the flat table using
`starschemar` package is as follows:

``` r
library(starschemar)
library(tidyr)

# columns to consider in the definition
dput(colnames(ft))
#> c("Year", "WEEK", "Week Ending Date", "REGION", "State", "City", 
#> "Pneumonia and Influenza Deaths", "All Deaths", "Other Deaths"
#> )

dm <- dimensional_model() %>%
  define_fact(
    name = "mrs_cause",
    measures = c(
      "Pneumonia and Influenza Deaths",
      "Other Deaths"
    ),
  ) %>%
  define_dimension(
    name = "when",
    attributes = c(
      "Week Ending Date",
      "WEEK",
      "Year"
    )
  ) %>%
  define_dimension(
    name = "where",
    attributes = c(
      "REGION",
      "State",
      "City"
    )
  )

st <- star_schema(ft, dm) %>%
  snake_case() %>%
  character_dimensions(
    NA_replacement_value = "Unknown",
    length_integers = list(week = 2)
  )
```

The tables of dimensions and facts of the obtained star scheme are shown
below.

| when\_key | week\_ending\_date |  week   |  year   |
| :-------: | :----------------: | :-----: | :-----: |
|     1     |     1962-01-06     |   01    |  1962   |
|     2     |     1962-01-13     |   02    |  1962   |
|     3     |     1962-01-13     | Unknown | Unknown |
|     4     |     1962-01-20     |   03    |  1962   |

| where\_key | region | state |    city    |
| :--------: | :----: | :---: | :--------: |
|     1      |   1    |  CT   | Bridgepor  |
|     2      |   1    |  CT   | Bridgeport |
|     3      |   9    |  WA   |   Tacoma   |

| when\_key | where\_key | pneumonia\_and\_influenza\_deaths | other\_deaths | nrow\_agg |
| :-------: | :--------: | :-------------------------------: | :-----------: | :-------: |
|     1     |     2      |                 3                 |      43       |     1     |
|     1     |     3      |                 4                 |      46       |     1     |
|     2     |     2      |                 2                 |      41       |     1     |
|     3     |     3      |                 2                 |      43       |     1     |
|     4     |     1      |                 2                 |      38       |     1     |
|     4     |     3      |                 0                 |      39       |     1     |

The tables show the erroneous and missing data. We are going to perform
some data cleaning operations to correct them.

``` r
where <- st %>%
  get_dimension("where")

when <- st %>%
  get_dimension("when")

updates <- record_update_set() %>%
  update_selection(
    dimension = where,
    columns = c("city"),
    old_values = c("Bridgepor"),
    new_values = c("Bridgeport")
  ) %>%
  match_records(dimension = when,
                old = 3,
                new = 2)

st <- st %>%
  modify_dimension_records(updates)
```

The new dimension and fact tables are shown below.

| when\_key | week\_ending\_date | week | year |
| :-------: | :----------------: | :--: | :--: |
|     1     |     1962-01-06     |  01  | 1962 |
|     2     |     1962-01-13     |  02  | 1962 |
|     3     |     1962-01-20     |  03  | 1962 |

| where\_key | region | state |    city    |
| :--------: | :----: | :---: | :--------: |
|     1      |   1    |  CT   | Bridgeport |
|     2      |   9    |  WA   |   Tacoma   |

| where\_key | when\_key | pneumonia\_and\_influenza\_deaths | other\_deaths | nrow\_agg |
| :--------: | :-------: | :-------------------------------: | :-----------: | :-------: |
|     1      |     1     |                 3                 |      43       |     1     |
|     1      |     2     |                 2                 |      41       |     1     |
|     1      |     3     |                 2                 |      38       |     1     |
|     2      |     1     |                 4                 |      46       |     1     |
|     2      |     2     |                 2                 |      43       |     1     |
|     2      |     3     |                 0                 |      39       |     1     |

In addition to the operations in the examples shown, `starschemar`
offers support for defining role playing and role dimensions in a star
schema, fact constellations with conformed dimensions, incremental data
refresh operations, and the ability to export results in various
`tibble`-based formats.
