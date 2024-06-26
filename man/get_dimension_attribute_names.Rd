% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/star_schema_rename.R
\name{get_dimension_attribute_names}
\alias{get_dimension_attribute_names}
\alias{get_dimension_attribute_names.star_schema}
\title{Get dimension attribute names}
\usage{
get_dimension_attribute_names(st, name)

\method{get_dimension_attribute_names}{star_schema}(st, name)
}
\arguments{
\item{st}{A \code{star_schema} object.}

\item{name}{A string, name of the dimension.}
}
\value{
A vector of attribute names.
}
\description{
Get the name of attributes in a dimension.
}
\examples{

attribute_names <-
  st_mrs_age |> get_dimension_attribute_names("when")

}
\seealso{
Other rename functions: 
\code{\link{get_measure_names}()},
\code{\link{rename_dimension}()},
\code{\link{rename_dimension_attributes}()},
\code{\link{rename_fact}()},
\code{\link{rename_measures}()}
}
\concept{rename functions}
