\name{choroplotPlan}
\alias{choroplotPlan}
\title{Create a choropleth (shaded thematic map) plot for a redistricting plan}
\description{
Plots a shaded thematic map on a plan, based on a set of schore
}


\usage{
  choroplotPlan(plan,scores,numlevels=5,
	method=c("quant","equal","absolute"),
	main="choropleth map", absmin=0,absmax=1,ramplow="blue",ramphigh="red",...)
}



\arguments{
\item{plan}{plan to plot}
\item{scores}{vector of scores}
\item{method}{binning method}
\item{main}{main title of plot}
\item{numlevels}{number of bins}
\item{absmin}{if method is \code{absolute} minimum for absolute range}
\item{absmax}{if method is \code{absolute} maximum for absolute range}
\item{ramplow}{color ramp endpoint for negative values}
\item{ramphigh}{color ramp endpoint for positive values}
\item{\dots}{arguments to pass on to map plotting}
}

\details{
	The scores should represent values for each district in the plan. Each score is assigned to one of \code{numlevels} bins. Districts are plotted according to the bin they are assigned to, using a color ramp (or a double color ramp for ranges that extend from negative to positive).
	
	Three methods of bin construction are supported:
	- absolute: creates equally spaced bins from \code{absmin} to \code{absmax}
	- equal: creates equally spaced bins from \code{min(scores)} to \code{max(scores)}
	- quant: bins are created using quantiles
}

\value{
  Nothing. The function is used for printing and plotting effect.
}

\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah\_altman/} 
}


\seealso{ 
  Scoring functions: \code{\link{calcContiguityScore}}
}
 
\examples{
  data(suffolk.map)
    
  # choose number of districts
  ndists <- 5

  # create some initial plans
  kplan <- createKmeansPlan(suffolk.map,ndists)
  choroplotPlan(kplan,calcPopScore(kplan),numlevels=3)


}
\keyword{IO}
\keyword{spatial}
\keyword{optimize}
\keyword{models}
\keyword{distribution}
