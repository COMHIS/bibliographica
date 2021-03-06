% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harmonize_gender.R
\name{harmonize_gender}
\alias{harmonize_gender}
\title{Harmonize Gender}
\usage{
harmonize_gender(x)
}
\arguments{
\item{x}{A vector of gender codes}
}
\value{
Harmonized vector
}
\description{
Harmonize commonly used gender terms
}
\examples{
x2 <- harmonize_gender(c("male", "female", "either"))
}
\references{
See citation("bibliographica")
}
\author{
Leo Lahti \email{leo.lahti@iki.fi}
}
\keyword{utilities}
