\name{createRandomPlan}
\alias{createRandomPlan}
\alias{createRandomPopPlan}
\alias{createKmeansPlan}
\alias{createAssignedPlan}
\alias{createContiguousPlan}
\alias{createContiguousDistrict}
\alias{createWeightedKmeansPlan}
\alias{createGreedyContiguousPlan}
\title{Create an initial plan for later refinement}
\description{
These methods create plans starting with random seeds. These plans should
be refined with one of the refinement functions.
}
\usage{
createRandomPlan(basemap,ndists)
createRandomPopPlan(basemap,ndists,predvar="POP")
createAssignedPlan(basemap,predvar="BARDPlanID",nseats=NULL,magnitudes=NULL)
createKmeansPlan(basemap,ndists)
createWeightedKmeansPlan( basemap, ndists, centers = c(), weightVar = NULL,
                 trimfactor = 2.5, smallBlock = c("cap", "closest"))
createContiguousPlan(basemap, ndists, predvar = "POP", threshold = 0.05,
                 ssize = 20, usebb = TRUE, maxtries = (10/threshold),
                 neighborstarts = TRUE, fillholes = TRUE, districtonly=FALSE, traceLevel=0)
createContiguousDistrict(basemap, ndists, predvar = "POP", threshold = 0.05,
                 ssize = 20, usebb = TRUE, maxtries = (10/threshold),
                 neighborstarts = TRUE, fillholes = TRUE)
createGreedyContiguousPlan( basemap, ndists, predvar="POP", scoreFUN=calcPopScore,display=FALSE,...)
}

\arguments{
 \item{basemap}{Bard basemap from which to create plan}
 \item{ndists}{number of districts to create} 
 \item{predvar}{name of variable of interest (for createAssignedPlan can be a vector or block assignments)} 
 \item{threshold}{tolerance threshold for districts being "equal" in population}
 \item{ssize}{check up to ssize neighbors at random, shoosing the one with the fewest foreign neighbors}
  \item{usebb}{use bounding box compactness}
  \item{maxtries}{maximum number of tries to create plan}
  \item{neighborstarts}{whether to start new district at neighbors of existing districts}
  \item{fillholes}{fill any holes remaining}
  \item{centers}{suggested centers for weighted kmeans}
  \item{weightVar}{name of weighting variable in basemap, for weighting}
  \item{trimfactor}{rescales and trims weights, see below }
  \item{smallBlock}{how weighted kmeans handles blocks that are weighted to 0: caps minimimum weights at 1; closest post-assigns zero-weighted blocks to the nearest district}
  \item{districtonly}{whether to generate a single district of the plan only}
  \item{scoreFUN}{Bard score function}
  \item{display}{whether to display plans while creating -- for demos and teaching}
  \item{traceLevel}{for debugging, tracelevel > 2 will turn on dynamic plotting}
  \item{nseats}{total seats (elected officials) represented by plan -- for partial assignments}
  \item{magnitudes}{a vector of length equal to the number of different districts in the assigned plan, with magnitudes of all districts -- for multimember plans only}
  \item{\dots}{arguments for score function}
}

\details{

\code{createKmeansPlan} create plans  through applying kmeans to the center of each polygon. It tends toward contiguity, but is not guaranteed to produce contiguous plans

\code{createRandomPlan} creates plans by random block assignment, as per Grofman [1982], these are highly non-contiguous

\code{createRandomPopPlan} creates plans by random block assignment, but caps each district at a population threshold. It will not add a block to a district in formation that will cause the plan to
go more than (1+threshold)/(totalpopulation/plan), unless that block would cause all
districts to exceed their thresholds.  With blocks of single individuals, and a threshold of 0 this is equivalent to the random assignment discusses. in Bush v. Vera [2004]

\code{createGreedyContiguousPlan} uses \code{createRandomPopPlan} followed by \code{fixContiguityPlan}

\code{createContiguousPlan} duplicates the algorthim in Cirincione, et. al (see the references). As per
pg 196:

``The first algorithm, the contiguity algorithm, begins by
randomly selecting a block group to serve as the \emph{base} of
the first district. It then constructs a \emph{perimeter list}
containing the unassigned block groups contiguous to the base block group.
The program then randomly selects a block group from the perimeter 
list to add to the emerging district and adjusts the perimeter list. 
This process continues until the population of the emerging district
achieves the desired district population level. (A newly 
created district is thrown out if its population deviates by more than 1%
from the ideal district average population, which in this case is 581,117.)
The next district begins with the random selection of a census block group
from among those that touch one of the completed districts.''

(Note that ssize was not included as an option, originally) 

In practice this method gets sometimes fails because creation of earlier districts prevents the creation of contiguous later districts. In this case, the method restarts, up to the maximum number of retstarts. If \code{fillholes} is true, then even a final failed attempt will be patched with \code{fixUnassignedPlan} and \code{fixContiguousPlan}, however, beware that this may yield some districts as empty, or districts out of the target criteria range. In this case a warning is issued. 

The \code{districtonly} option will cause a single district only to be created, which is a quick-and-dirty way of pseudo-sampling districts. 

The resulting plan object is flagged as a `single-district' plan and score functions will return the score for that district, not the plan as a whole. 

\code{createContiguousDistrict} is a wrapper for \code{createContiguousPlan} with the districtonly option set. 

\code{createWeightedKmeansPlan} weights kmeans by a particular variable, most often used with population. This is implemented through block replication, so to limit memory use, the weights are rescaled so that minimum and maximum weights span approximately \code{trimfactor} orders of magnitude. Zero-weight blocks can either be capped at 1 an incorporated in the kmeans calculation, or left out and assigned to the closest district, based on the value of \code{smallblock}.


}

\note{
	Turning on dynamic plotting is great for teaching and demos, but slows down plan creation by several orders of magnitude.
}

\value{
  Returns a bard plan. Note that this plan is not guaranteed to be contiguous, or equipopulous.
}

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
Interactive editing \code{\link{editPlanInteractive}}
Plan refinement algorithms \code{\link{refineGreedyPlan}},  \code{\link{refineAnnealPlan}}, \code{\link{refineGenoudPlan}}, \code{\link{refineNelderPlan}}.
Plan fixups \code{\link{fixUnassignedPlan}} , \code{\link{fixContiguityPlan}}
 }
 
\examples{
  # read in a shapefile with demographic data
  data(suffolk.map)
    
    
  # choose number of districts
  ndists <- 5
  
  # create some initial plans
  kplan <- createKmeansPlan(suffolk.map,ndists)
  rplan <- createRandomPlan(suffolk.map,ndists)
  aplan <- createAssignedPlan(suffolk.map,predvar=1:ndists)
  wkplan<-createWeightedKmeansPlan(suffolk.map,ndists,weightVar="POP")

  
  cplan <- createContiguousPlan(suffolk.map,ndists)
  plot(cplan)
  summary(cplan)
  
 
  plot(kplan)
  summary(kplan)
  plot(summary(rplan))
  
  
  c1plan<-createGreedyContiguousPlan(suffolk.map,ndists)
  
  plot(c1plan)
  summary(c1plan)
  
  # single district only 
  c2plan<-createContiguousDistrict(suffolk.map,ndists)
  c2plan
  plot(c2plan)
  summary(c2plan)
  
}

\keyword{IO}
\keyword{spatial}
\keyword{optimize}
\keyword{models}
\keyword{distribution}
