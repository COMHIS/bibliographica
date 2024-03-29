% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mark_dissertations.R
\name{mark_dissertations}
\alias{mark_dissertations}
\title{mark_dissertations}
\usage{
mark_dissertations(x)
}
\arguments{
\item{x}{dissertation field (a vector)}
}
\value{
data.frame with fields for regular disserations and for synodal dissertations
}
\description{
Establish two binary variables: dissertation and synodal dissertation
}
\examples{
\dontrun{df <- mark_dissertations(c("Diss.","Diss. synod"))}
}
\references{
See citation("bibliographica")
}
\author{
Niko Ilomaki \email{niko.ilomaki@helsinki.fi}
}
\keyword{utilities}
