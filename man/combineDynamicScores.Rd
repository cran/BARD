\name{combineDynamicScores}
\alias{combineDynamicScores}
\title{Convenience function for combining score functions.}
\description{
This convenience function combines multiple dynamic score functions, while doing the housekeeping to track dynamic updates across each function.
}
\usage{
combineDynamicScores(plan,lastscore=NULL,changelist=NULL,scorefuns=list(),
	distcombfun=sum,scorecombfun=sum)
}

\arguments{
 \item{plan}{plan to be scored}
 \item{lastscore}{optional, previous value returned by function, for incremental evaluation}
 \item{changelist}{Optional, a two column matrix of (column1) block ID's that were changed since lastscore was computed, (column2) previous plan assignments for those blocks} 
 \item{scorefuns}{list of score functions}
 \item{distcombfun}{function used to combine scores across districts} 
  \item{scorecombfun}{function for combining scores into a single score}
}

\details{

All of the bard score functions implement some sort of dynamic update for efficiency
when scoring large plans. The refinement methods make use of these dynamic score functions. The score can be dynamically recalculated based on the 
last score returned, accompanied by a list of changes. Dynamic recalculation is optional. Note that in order to support dynamic recalculation, the score vector returned may have additional attached attributes.

This function combined multiple dynamic score functions correctly -- by multiplexing and de-multiplexing the attributes, so that each score function gets the appropriate tracking information.
}

\value{
	Returns a single score for the plan, plus attributes for dynamic recalculation.
}

\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://maltman.hmdc.harvad.edu/} 
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
	Plan refinement algorithms \code{\link{refineGreedyPlan}},  \code{\link{refineAnnealPlan}}, \code{\link{refineGenoudPlan}}, \code{\link{refineNelderPlan}}, \code{\link{refineTabuPlan}}
 }
 
\examples{
  # read in a shapefile with demographic data
  data(suffolk.map)
  ndists<-5

  
  # create some initial plans
  kplan <- createKmeansPlan(suffolk.map,ndists)
  
  calcPopScore(kplan)
  calcLWCompactScore(kplan)
  calcRangeScore(kplan, targrange=c(.01,.99))

combineDynamicScores(kplan,scorefuns=list(calcPopScore,calcLWCompactScore))

myScore<-function(plan,...){
   combineDynamicScores(plan,scorefuns=list(
        calcPopScore,
        calcLWCompactScore,
        calcContiguityScore,
        function(x,...)calcRangeScore(x,targrange=c(.01,.99),...)
   ))
}

myScore(kplan)

       kplan2<-kplan1<-kplan
       cl<-cbind(c(318,320),c(kplan1[318],kplan1[320]))
       if ( ! all(calcPopScore(kplan2,lastscore=calcPopScore(kplan1),changelist=cl) == calcPopScore(
kplan2))) {
             warning("dynamic score does not match!")
       }

        kplan2[318]<-(kplan1[318]+1)%%ndists+1
        kplan2[320]<-(kplan1[320]+1)%%ndists+1

        oldscore<-myScore(kplan1)
 if ( ! all (myScore(kplan2) ==  myScore(kplan2,lastscore=oldscore,changelist=cl))){
             warning("dynamic score does not match!")
       }


}

\keyword{IO}
\keyword{spatial}
\keyword{optimize}
\keyword{models}
\keyword{distribution}
