\name{editPlanInteractive}
\alias{editPlanInteractive}
\alias{createPlanInteractive}
\title{ Create and edit plans interactively, with a GUI}
\description{
  These functions allow one to create or edit a plan interactively, by selecting blocks from a map and assigning them to districts. Reports can be generated for each selection.
}
\usage{
editPlanInteractive(plan, reportFUN = NULL, ...)
createPlanInteractive(basemap, ndists, reportFUN = NULL, ...)
}

\arguments{
  \item{plan}{plan to be edited}
  \item{basemap}{bard basemap}
  \item{ndists}{number of districts}
  \item{reportFUN}{this function will be called after every selection is
	added, for continuous reporting of district scores}
  \item{\dots}{other arguments to pass to reportFUN}
}
\value{
	returns a bard plan
}

\section{warning}{This depends on the iplots package. This package is still beta-quality. You may see occasional glitches. And on Macintosh systems, iplots must be run under JGR }
\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/} 
 }

 \references{
    Micah Altman, 1997. ``Is Automation the Answer? The Computational Complexity of Automated Redistricting'', Rutgers Computer and Technology Law Journal 23 (1), 81-142
  \url{http://www.hmdc.harvard.edu/micah\_altman/pubpapers.shtml}
  
  Altman, M. 1998. Modeling the Effect of Mandatory District Compactness on Partisan Gerrymanders, \emph{Political Geography} 17:989-1012.
  
  Bush v. Vera, 517 U.S. 952 (1996).
  
    Micah Altman and Michael P. McDonald. 2004. A Computation Intensive Method for Detecting Gerrymanders  Paper presented at the annual meeting of the The Midwest Political Science Association, Palmer House Hilton, Chicago, Illinois, Apr 15, 2004. \url{http://www.allacademic.com/meta/p83108\_index.html}

  
C. Cirincione , T.A. Darling, and T.G. O'Rourke. 2000. ``Assessing South Carolina's 1990's Congressional Districting.'' Political Geography 19: 189-211.
  
  Grofman, B. 1982, "For single Member Districts Random is Not Equal", In \emph{Representation and Redistricting Issues}, ed. B. Grofman, A. Lijphart, R. McKay, H. Scarrow. Lexington, MA: Lexington Books.
  
  Micah Altman, Karin Mac Donald, and Michael P. McDonald, 2005. ``From Crayons to Computers: The Evolution of Computer Use in Redistricting'' Social Science Computer Review 23(3): 334-46. 
  
 }

\seealso{ 
Other plan creation functions \link{createRandomPlan}, etc.
Plan refinement algorithms \code{\link{refineGreedyPlan}},  \code{\link{refineAnnealPlan}}, \code{\link{refineGenoudPlan}}, \code{\link{refineNelderPlan}}}
 
\examples{
  # read in a shapefile with demographic data
 data(suffolk.map)
    
    
  # choose number of districts
  ndists <- 5
  
  # create some initial plans
  kplan <- createKmeansPlan(suffolk.map,ndists)
  \dontrun{
  if (require("iplots",quietly=TRUE)) {
  	kplan<-editPlanInteractive(kplan, reportFUN=function(x)print(calcPopScore(x))) 
  }
  }
}

\keyword{IO}
\keyword{spatial}
\keyword{optimize}
\keyword{models}
\keyword{distribution}
