% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/star_schema_as_multistar.R
\name{star_schema_as_multistar}
\alias{star_schema_as_multistar}
\alias{star_schema_as_multistar.star_schema}
\title{Export a star schema as a \code{multistar}}
\usage{
star_schema_as_multistar(st)

\method{star_schema_as_multistar}{star_schema}(st)
}
\arguments{
\item{st}{A \code{star_schema} object.}
}
\value{
A \code{multistar} object.
}
\description{
Once we have refined the format or content of facts and dimensions, we can
obtain a \code{multistar}. A \code{multistar} only distinguishes between general and
conformed dimensions, each dimension has its own data. It can contain
multiple fact tables.
}
\examples{

ms <- st_mrs_age |>
  star_schema_as_multistar()

}
\seealso{
Other results export functions: 
\code{\link{constellation_as_multistar}()},
\code{\link{constellation_as_tibble_list}()},
\code{\link{multistar_as_flat_table}()},
\code{\link{star_schema_as_flat_table}()},
\code{\link{star_schema_as_tibble_list}()}
}
\concept{results export functions}
