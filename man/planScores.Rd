\name{calcLWCompactScore}
\alias{calcLWCompactScore}
\alias{calcPACompactScore}
\alias{calcUnassignedScore}
\alias{calcSpatialHolesScore}
\alias{calcPopScore}
\alias{calcRangeScore}
\alias{calcGroupScore}
\alias{calcReockScore}
\alias{calcHolesScore}
\alias{calcBBCompactScore}
\alias{calcMomentScore}
\alias{calcSplitScore}
\alias{calcIneqScore}
\alias{calcContiguityScore}
\title{Scoring functions for redistricting.}
\description{
These functions evaluate redistricting plans. 
}
\usage{
calcRangeScore(plan, predvar="BLACK", predvar2="WHITE", targrange=c(.65,.70),
  sumdenom = TRUE,lastscore=NULL, changelist=NULL, standardize=TRUE )
  
calcGroupScore(plan,groups=list(),penalties=1,lastscore=NULL, changelist=NULL, standardize=TRUE)

calcPopScore(plan, predvar="POP",lastscore=NULL, changelist=NULL, standardize=TRUE )

calcPACompactScore(plan, lastscore=NULL, changelist=NULL, standardize=TRUE )
calcLWCompactScore(plan, lastscore=NULL, changelist=NULL, standardize=TRUE )
calcReockScore(plan, usebb=FALSE, lastscore=NULL, changelist=NULL, standardize=TRUE )

calcContiguityScore(plan, lastscore=NULL, changelist=NULL, standardize=TRUE )

calcBBCompactScore(plan, lastscore=NULL, changelist=NULL, standardize=TRUE )

calcUnassignedScore(plan, lastscore = NULL, changelist = NULL, standardize = TRUE)

calcHolesScore(plan, lastscore = NULL, changelist = NULL, standardize = TRUE)
calcSpatialHolesScore(plan, lastscore = NULL, changelist = NULL, standardize = TRUE)

calcMomentScore(plan,standardize=TRUE,centers=NULL,weightVar=NULL,penaltyExp=2,  normalize=TRUE, lastscore=NULL, changelist=NULL ) 

calcSplitScore(plan, splitvar, lastscore = NULL, changelist = NULL, standardize = TRUE)

calcIneqScore(plan, eqvar, weightVar = NULL, parameter = NULL, type =c("Gini", "RS", "Atkinson", "Theil", "Kolm", "var","square.var", "entropy"), lastscore = NULL, changelist = NULL, standardize = TRUE)


}

\arguments{
 \item{plan}{plan to be scored}
 \item{lastscore}{optional, previous value returned by function, for incremental evaluation}
 \item{changelist}{Optional, a two column matrix of (column1) block ID's that were changed since lastscore was computed, (column2) previous plan assignments for those blocks} 
 \item{standardize}{logical, should scores be standardized}
 \item{penalties}{penalties for splitting groups}
 \item{predvar}{name of variable in the basemap associated with plan} 
  \item{predvar2}{name of second variable in the basemap associated with plan}
  \item{targrange}{acceptable target range for ratio of predictive variables}
  \item{groups}{a list of vectors, each vector should comprise the ids of the blocks  in that group, groups may overlap}
  \item{penaltyExp}{a single number or vector of penalties for splitting each of the enumerated groups}
  \item{centers}{optional centers to use for calculating moment of inertia score, if absent, score is calculated using (weighted) district centroid (centroid of block centroids)}
  \item{weightVar}{name of variable in basemap to use for weighting}
  \item{sumdenom}{For calcRangeScore, whether to use the sum of both variables as the denominator, or only use the second variable}
  \item{usebb}{For \code{calcReockScore} use bounding boxes, instead of the entire polygon. This is a fast approximation, which is accurate where the polygons are much smaller than the district.}
  \item{normalize}{Flag -- normalize moment of inertia score}
  \item{splitvar}{Variable containing geographic id, such as county id}
  \item{eqvar}{variable to be used for homogeneity score}
  \item{type}{type of inequality measure -- see \code{ineq}}
  \item{parameter}{parameter for inequality measuer -- see \code{ineq}}
  
}

\details{

calcContiguityScore- returns a score based on the number of separate contiguous regions in the district. The ideal district comprises a single contiguous region.

calcLWCompactScore -- Returns a compactness score based on the ratio of the sides of the bounding rectangle for the district.

calcReockScore - Returns a compactness score based on the ratio of area to area of a circle.The \code{usebb} option uses polygon bounding boxes, which is much faster, and  accurate if the polygons are small relative to the district.  

calcBBCompactScore - Returns a compactness score based on the ratio of area to area of bounding box.Similar to Reock but much faster

calcPopScore - Returns a score based on the deviation from population equality of the districts. 

calcRangeScore -  Calculates disctricts compliance to a target range for a predictive variable. Will penalize districts increasingly as sum(prevar1)/(sum(predvar1)+sum(predvar2)) (if sumdenom==TRUE) falls outside the given target range. Use this for majority-minority districts, partisan districts, competitive districts

calcGroupScore -  Calculates plans compliance with keeping designated groups. Use for 
designated "nesting" districts, or known communities of interest. Returns proportion of group split by a district (if a group is split across multiple districts, all districts are penalized).

calcUnassignedScore - Penalizes plans for having unassigned blocks
calcHolesScore - alias for calcUnassignedScore

calcSpatialHolesScore - penalizes plans for number of spatial (donut) holes (i.e. topological genus) in contiguous portions of the plan. (Completely separate portions of plans are not considered to have holes -- but are counted by calcContiguityScore. 

calcPACompactScore - calculates plan compactness as a perimeter/ration area (where a Circle has a perfect score of 0)

calcMomentScore - calculates moment of inertia compactness score of the district, the sum or squared distance of the block centroids (weighted by are of the block) to the district centroids. Options allow changing the penalty exponent (e.g. penaltyExp=1 will sum distances), assign a weightVar which will be used instead of area (e.g. population, for population moment of inertia), or specify fixed district centers to use as a substitute for district centroid (e.g. to use for warehouse allocation problems) . The normalization score gives the unitless, normalized version which is insensitive to scale (a default in version 1.17)
}
\note{

All of the bard score functions implement some sort of dynamic update for efficiency
when scoring large plans. The refinement methods make use of these dynamic score functions. The score can be dynamically recalculated based on the 
last score returned, accompanied by a list of changes. Dynamic recalculation is optional. Note that in order to support dynamic recalculation, the score vector returned may have additional attached attributes.

Incremental updating is not required of user-supplied score functions. Bard functions functions will check for the existence of a lastscore argument to determine whether this is available.
}

\value{
All current plan score functions, except calcUnassignedScorel return a vector of score value, representing the score for each district, with the plan score being the sum of this vector.

CalcUnassignedScore returns a single score for the plan. User-written score functions MAY also return a single number as a plan score instead, all bard utilities will handle this case correctly.

If the the "standardize" option is true. Each of these values should be       
standardized to  [0,1], with 0  representing  the "best" score and 1 the worst score Otherwise,   score values MAY return values in other scalar  ranges, and even invert the ranking. It is recommended that standardized scored be used except for testing.

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
  
  Altman, Micah, 1998. Districting Principles and Democratic Representation,
Doctoral Thesis, California Institute of Technology.
  
  Niemi, R. G.; Grofman, B.; Carlucci, C.; and Hofeller T., 1990. Measuring Compactness and the Role of a Compactness Standard in a Test for Partisan and Racial Gerrymandering Journal of Politics 22:4 1155-1181.
  
  
 }

 \section{warning}{
 	 calcSpatialHoles score will produce correct results only where the underlying districting units do not have spatial holes that fully contain other units. This is usually true of census blocks but not census tracts.
 }
 
 
\seealso{ 
Plan refinement algorithms \code{\link{refineGreedyPlan}},  \code{\link{refineAnnealPlan}}, \code{\link{refineGenoudPlan}},
\code{\link{refineTabuPlan}},
\code{\link{refineNelderPlan},}.

Combining dynamic scores \code{\link{combineDynamicScores}}.

 
 Inequality package \code{\link[ineq]{ineq}}
} 
\examples{
  # read in a shapefile with demographic data
data(suffolk.map)
  ndists<-5
  
  # create some initial plans
  kplan <- createKmeansPlan(suffolk.map,ndists)
  rplan <- createRandomPlan(suffolk.map,ndists)
  wkplan<-createWeightedKmeansPlan(suffolk.map,ndists,weightVar="POP")
  
  calcPopScore(kplan)
  calcPopScore(rplan)
  calcLWCompactScore(rplan)
  calcLWCompactScore(kplan)
  calcPACompactScore(rplan)
  calcPACompactScore(kplan)
  calcUnassignedScore(kplan)
  calcUnassignedScore(rplan)
  calcSpatialHolesScore(kplan)
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
 
  if ( ! all(calcPopScore(kplan2,lastscore=calcPopScore(kplan1),changelist=cl) == calcPopScore(kplan2))) {
    	warning("dynamic score does not match!")
  }
    
  calcPopScore(kplan,predvar="POP")
  calcPopScore(wkplan,predvar="POP")
  calcMomentScore(kplan)
  calcMomentScore(wkplan)
  calcMomentScore(kplan,weightVar="POP")
  calcMomentScore(wkplan,weightVar="POP")


}

\keyword{IO}
\keyword{spatial}
\keyword{optimize}
\keyword{models}
\keyword{distribution}
