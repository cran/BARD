\name{calcLWCompactScore}
\alias{calcLWCompactScore}
\alias{calcPopScore}
\alias{calcRangeScore}
\alias{calcGroupScore}
\alias{calcReockScore}
\alias{calcHolesScore}
\alias{calcContiguityScore}
\title{Scoring functions for redistricting.}
\description{
These functions evaluate redistricting plans. 
}
\usage{
calcRangeScore(plan, predvar="BLACK", predvar2="WHITE", targrange=c(.65,.70),
  lastscore=NULL, changelist=NULL, standardize=TRUE )
calcGroupScore(plan,groups=list(),penalties=1,lastscore=NULL, changelist=NULL, standardize=TRUE)
calcPopScore(plan, predvar="POP",lastscore=NULL, changelist=NULL, standardize=TRUE )
calcLWCompactScore(plan, lastscore=NULL, changelist=NULL, standardize=TRUE )
calcReockScore(plan, lastscore=NULL, changelist=NULL, standardize=TRUE )
calcContiguityScore(plan, lastscore=NULL, changelist=NULL, standardize=TRUE )
calcHolesScore(plan, lastscore = NULL, changelist = NULL, standardize = TRUE)
}

\arguments{
 \item{plan}{plan to be scored}
 \item{lastscore}{optional, previous value returned by function, for incremental evaluation}
 \item{changelist}{Optional, a two column matrix of (column1) block ID's that were changed since lastscore was computed, (column2) previous plan assignments for those blocks} 
 \item{standardize}{logical, should scores be standardized}
 \item{predvar}{name of variable in the basemap associated with plan} 
  \item{predvar2}{name of second variable in the basemap associated with plan}
  \item{targrange}{acceptable target range for ratio of predictive variables}
  \item{groups}{a list of vectors, each vector should comprise the ids of the blocks  in that group, groups may overlap}
  \item{penalties}{a single number or vector of penalties for splitting each of the enumerated groups}
}

\details{

calcContiguityScore- returns a score based on the number of separate contiguous regions in the district. The ideal district comprises a single contiguous region.

calcLWCompactScore -- Returns a compactness score based on the ratio of the sides of the bounding rectangle for the district.

calcReockScore - Returns a compactness score based on the ratio of area to area of a circle.

calcPopScore - Returns a score based on the deviation from population equality of the districts. 

calcRangeScore -  Calculates disctricts compliance to a target range for a predictive variable. Will penalize districts increasingly as sum(prevar1)/(sum(predvar1)+sumpredvar(2)) falls outside the given target range. Use this for majority-minority districts, partisan districts, competitive districts

calcGroupScore -  Calculates plans compliance with keeping designated groups. Use for 
designated "nesting" districts, or known communities of interest. Returns proportion of group split by a district (if a group is split across multiple districts, all districts are penalized).

calcHolesScores - Penalizes plans for having unassigned blocks

}
\note{

All of the bard score functions implement some sort of dynamic update for efficiency
when scoring large plans. The refinement methods make use of these dynamic score functions. The score can be dynamically recalculated based on the 
last score returned, accompanied by a list of changes. Dynamic recalculation is optional. Note that in order to support dynamic recalculation, the score vector returned may have additional attached attributes.

Incremental updating is not required of user-supplied score functions. Bard functions functions will check for the existence of a lastscore argument to determine whether this is available.
}

\value{
All current plan score functions return a vector of score value, representing the score for each district, with the plan score being the sum of this vector. (User-written score functions MAY return a single number as a plan score instead, all bard utilities will handle this case correctly.)

If the the "standardize" option is true. Each of these values should be       
standardized to  [0,1], with 0  representing  the "best" score and 1 the worst score
Otherwise,   score values MAY return values in other scalar  ranges, and even invert the ranking. It is recommended that standardized scored be used except for testing.

Note that in order to support dynamic recalculation, the score vector returned may have additional attached attributes.
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
  
C. Cirincione , T.A. Darling, and T.G. O'Rourke. 2000. ``Assessing South Carolina's 1990's Congressional Districting.'' Political Geography 19: 189-211.
  
  Micah Altman and Michael P. McDonald. 2004. A Computation Intensive Method for Detecting Gerrymanders  Paper presented at the annual meeting of the The Midwest Political Science Association, Palmer House Hilton, Chicago, Illinois, Apr 15, 2004. \url{http://www.allacademic.com/meta/p83108\_index.html}

  Micah Altman, Karin Mac Donald, and Michael P. McDonald, 2005. ``From Crayons to Computers: The Evolution of Computer Use in Redistricting'' Social Science Computer Review 23(3): 334-46. 
  
 }

\seealso{ 
Plan refinement algorithms \code{\link{refineGreedyPlan}},  \code{\link{refineAnnealPlan}}, \code{\link{refineGenoudPlan}}, \code{\link{refineNelderPlan}}
 }
 
\examples{
  # read in a shapefile with demographic data
  suffolk.map <- importBardShape(
    system.file("shapefiles/suffolk_tracts.shp", package="BARD"))
  ndists<-5
  
  # create some initial plans
  kplan <- createKmeansPlan(suffolk.map,ndists)
  rplan <- createRandomPlan(suffolk.map,ndists)
  
  calcPopScore(kplan)
  calcPopScore(rplan)
  calcLWCompactScore(kplan)
  calcLWCompactScore(rplan)
  calcLWCompactScore(kplan)
  calcContiguityScore(rplan)
  calcContiguityScore(kplan)
  calcRangeScore(kplan)
  calcRangeScore(rplan)
  calcRangeScore(kplan, targrange=c(.01,.99))
  calcRangeScore(rplan, targrange=c(.01,.99))
  calcGroupScore(kplan,groups=list(c(1:10),c(100:120)),penalties=c(1,2))
  calcGroupScore(rplan,groups=list(c(1:10),c(100:120)),penalties=c(1,2))
  kplan2<-kplan1<-kplan
  cl<-cbind(c(318,320),c(kplan1[318],kplan1[320]))

  calcPopScore(kplan2,lastscore=calcPopScore(kplan1),changelist=cl) == 
    calcPopScore(kplan2)


}

\keyword{IO}
\keyword{spatial}
\keyword{optimize}
\keyword{models}
\keyword{distribution}
