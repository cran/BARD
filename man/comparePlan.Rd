\name{reportPlans}
\alias{reportPlans}
\title{evaluate a set of plans}
\description{
This function evaluates scores, and differences among a set of plans.
}


\usage{
  reportPlans(
  plans,
  scoreFUNs=list(
    "Contiguity"=calcContiguityScore,
    "Holes"=calcHolesScore,
    "LW Compact"=calcLWCompactScore,
    "Reock"=calcReockScore
  ),
  doplot=FALSE,
  domatch=TRUE,
  dodiff=TRUE,
  dodetails=FALSE,
  doprofileextras=TRUE,
  plotOpts=NULL,
  useHTML=FALSE,
  completeHTML=FALSE,
  ...
  )
}



\arguments{
\item{plans}{a list of bard plans.}
\item{scoreFUNs}{a list of named score functions that accept a plan as an argument, and return a vector of scores}
\item{domatch}{logical, whether to attempt to reorder district ID's to match}
\item{doplot}{ Logical.  Whether to plot differences.}
\item{dodetails}{Logical. Print detailed information}
\item{dodiff}{Logical. Report differences between pairs of plans}
\item{doprofileextras}{Logical. Report profile summaries for bardSample results}
\item{plotOpts}{List of plotting options to send to plan plotting command}
\item{useHTML}{use html formatting for reports}
\item{completeHTML}{produce a complete HTML file -- note the path is returned}
\item{\ldots}{arguments passed on to \code{print} and \code{HTML}}
}

\details{
This is the externally visible routine for comparing a list of plans. If multiple plans are given, each is compared against the first plan in the list.
}

\value{
  Nothing. The function is used for printing and plotting effect.... unless the completeHTML option is used, in which case returns the path the the HTML file, invisibly.

}

\note{Note the following limitation: all plans being compared must have the same number of districts and basemap} 

\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah\_altman/} 
}

\references{
   Micah Altman, 1997. ``Is Automation the Answer? The Computational Complexity of Automated Redistricting'', Rutgers Computer and Technology Law Journal 23 (1), 81-142
  \url{http://www.hmdc.harvard.edu/micah\_altman/pubpapers.shtml}
  
  Altman, M. 1998. Modeling the Effect of Mandatory District Compactness on Partisan Gerrymanders, \emph{Political Geography} 17:989-1012.
  
  Micah Altman and Michael P. McDonald. 2004. A Computation Intensive Method for Detecting Gerrymanders  Paper presented at the annual meeting of the The Midwest Political Science Association, Palmer House Hilton, Chicago, Illinois, Apr 15, 2004. \url{http://www.allacademic.com/meta/p83108\_index.html}

  Micah Altman, Karin Mac Donald, and Michael P. McDonald, 2005. ``From Crayons to Computers: The Evolution of Computer Use in Redistricting'' Social Science Computer Review 23(3): 334-46. 
}

\seealso{ 
  Scoring functions: \code{\link{calcContiguityScore}}
  Component functions: \code{\link{scorePlans}} , \code{\link{diff.bardPlan}}
}
 
\examples{
  data(suffolk.map)
    
  # choose number of districts
  ndists <- 5

  # create some initial plans
  kplan1 <- createKmeansPlan(suffolk.map,ndists)
  kplan2 <- createKmeansPlan(suffolk.map,ndists)
reportPlans(plans=list("kmeans"=kplan1,"kmeans 2"=kplan2), doplot=TRUE)

}
\keyword{IO}
\keyword{spatial}
\keyword{optimize}
\keyword{models}
\keyword{distribution}
