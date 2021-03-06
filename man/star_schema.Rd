% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/star_schema.R
\name{star_schema}
\alias{star_schema}
\title{\code{star_schema} S3 class}
\usage{
star_schema(ft, sd)
}
\arguments{
\item{ft}{A \code{tibble}, implements a flat table.}

\item{sd}{A \code{dimensional_model} object.}
}
\value{
A \code{star_schema} object.
}
\description{
Creates a \code{star_schema} object from a flat table (implemented by a \code{tibble})
and a \code{dimensional_model} object.
}
\details{
Transforms the flat table data according to the facts and dimension
definitions of the \code{dimensional_model} object. Each dimension is generated with
a surrogate key which is a foreign key in facts.

Facts only contain measurements and foreign keys.
}
\examples{

st <- star_schema(mrs_age, dm_mrs_age)

}
\seealso{
\code{\link{dimensional_model}}

Other star schema and constellation definition functions: 
\code{\link{character_dimensions}()},
\code{\link{constellation}()},
\code{\link{role_playing_dimension}()},
\code{\link{snake_case}()}
}
\concept{star schema and constellation definition functions}
