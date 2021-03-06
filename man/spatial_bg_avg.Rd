% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial_bg_avg.R
\name{spatial_bg_avg}
\alias{spatial_bg_avg}
\title{spatial_bg_avg}
\usage{
spatial_bg_avg(values = NULL, receptors = NULL, bg_geoids = NULL,
  results_only = FALSE)
}
\arguments{
\item{values}{Receptor modeling results for averaging.}

\item{receptors}{MNRISKS receptor number.}

\item{bg_geoids}{Census block group geoid numbers to include in results. Default returns all block groups.}
}
\description{
Find the spatial block group average, weighting each receptor by its spatial coverage of the block group.
}
\examples{
# For all block groups
bg_avg <- spatial_bg_avg(values    = df$concentration, 
                         receptors = df$receptors)
                         
}
\keyword{average}
\keyword{block}
\keyword{group}
\keyword{mnrisks}
\keyword{receptors}
\keyword{spatial}
