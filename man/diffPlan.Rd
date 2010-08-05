\name{scorePlans}
\alias{scorePlans}
\alias{diff.bardPlan}
\title{Methods for comparing plans}
\description{
	Compares plans by differences and such
}
\usage{
scorePlans(plans, scoreFUNs, domatch = TRUE)
\method{diff}{bardPlan}(x,plan2,domatch=TRUE,...)
}
\arguments{
  \item{plans}{ list of plans }
  \item{x}{first plan}
  \item{plan2}{second plan}
  \item{scoreFUNs}{ list of score functions}
  \item{domatch}{ rearrange district ID's for a best match between two plans}
  \item{\dots}{ignored}
}
\note{
	Use \code{summary}, and \code{plot(summary)} to display results
}
\value{
	Score plans returns a score data frame
	diff returns a difference lists
}
\examples{
  data(suffolk.map)
  numberdists <- 5
  kplan <- createKmeansPlan(suffolk.map,numberdists)
  rplan <- createRandomPlan(suffolk.map,numberdists)
  pdiff <- diff(kplan,rplan)
  # detailed
  print(pdiff)
  # numbers of changes
  print(summary(pdiff)) 
  # shows changed blocks on a map
  plot(pdiff,plotall=TRUE)
  # show scores
  scorePlans(list("kmeans"=kplan,"random"=rplan),
	scoreFUNs=list( "Contiguity"=calcContiguityScore,
    	"Holes"=calcHolesScore,
    "LW Compact"=calcLWCompactScore,
    "Reock"=calcReockScore) )
}

\seealso{ \code{\link{reportPlans}}}
\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/} 
 }

\keyword{spatial}
\keyword{models}
\keyword{distribution}
