% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/star_schema_rename.R
\name{rename_dimension}
\alias{rename_dimension}
\alias{rename_dimension.star_schema}
\title{Rename dimension}
\usage{
rename_dimension(st, name, new_name)

\method{rename_dimension}{star_schema}(st, name, new_name)
}
\arguments{
\item{st}{A \code{star_schema} object.}

\item{name}{A string, name of the dimension.}

\item{new_name}{A string, new name of the dimension.}
}
\value{
A \code{star_schema} object.
}
\description{
Set new name for a dimension.
}
\examples{

st <- st_mrs_age |>
  rename_dimension(name = "when", new_name = "when_happened")

}
\seealso{
Other rename functions: 
\code{\link{get_dimension_attribute_names}()},
\code{\link{get_measure_names}()},
\code{\link{rename_dimension_attributes}()},
\code{\link{rename_fact}()},
\code{\link{rename_measures}()}
}
\concept{rename functions}
