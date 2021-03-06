% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polish_field.R
\name{polish_field}
\alias{polish_field}
\title{Polish Field}
\usage{
polish_field(df, df.preprocessed, field, verbose = TRUE, languages = NULL)
}
\arguments{
\item{df}{data.frame that includes the given field}

\item{df.preprocessed}{Preprocessed data.frame that may contain auxiliary info.}

\item{field}{Field to be preprocessed.}

\item{verbose}{verbose}

\item{languages}{Languages to be used in polishing}
}
\value{
Output of the polished field
}
\description{
Polish a specified library catalogue field.
}
\details{
A wrapper bringing together the separate field processing methods into a single function.
}
\examples{
\dontrun{a <- polish_field(df, "title")}
}
\references{
See citation("bibliographica")
}
\author{
Leo Lahti \email{leo.lahti@iki.fi}
}
\keyword{utilities}
