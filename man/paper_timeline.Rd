% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paper_timeline.R
\name{paper_timeline}
\alias{paper_timeline}
\title{Paper Consumption Timeline}
\usage{
paper_timeline(x, field, nmin = 0)
}
\arguments{
\item{x}{data frame}

\item{field}{Field indicating the groups to compare}

\item{nmin}{Include only entries with at least nmin occurrences}
}
\value{
List:
\itemize{
  \item{plot}{ggplot object}
  \item{table}{summary table}
}
}
\description{
Compare paper consumption among selected groups
}
\examples{
\dontrun{paper_timeline(df, "gatherings")}
}
\references{
See citation("bibliographica")
}
\author{
Leo Lahti \email{leo.lahti@iki.fi}
}
\keyword{utilities}
