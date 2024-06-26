% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/star_schema_enrich_dimension.R
\name{enrich_dimension_export}
\alias{enrich_dimension_export}
\alias{enrich_dimension_export.star_schema}
\title{Export selected attributes of a dimension}
\usage{
enrich_dimension_export(st, name = NULL, attributes = NULL)

\method{enrich_dimension_export}{star_schema}(st, name = NULL, attributes = NULL)
}
\arguments{
\item{st}{A \code{star_schema} object.}

\item{name}{A string, name of the dimension.}

\item{attributes}{A vector of attribute names.}
}
\value{
A \code{tibble} object.
}
\description{
Export the selected attributes of a dimension, without repeated combinations,
to enrich the dimension.
}
\details{
If it is a role dimension they cannot be exported, you have to work with the
associated role playing dimension.
}
\examples{

tb <-
  enrich_dimension_export(st_mrs_age,
                          name = "when_common",
                          attributes = c("week", "year"))

}
\seealso{
Other dimension enrichment functions: 
\code{\link{enrich_dimension_import}()},
\code{\link{enrich_dimension_import_test}()}
}
\concept{dimension enrichment functions}
