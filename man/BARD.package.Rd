\name{BARD-package}
\alias{BARD-package}
\alias{BARD}
\docType{package}
\title{
A package for better automated redistricting.
}
\description{
BARD will automatically generate redistricting plans using multi-criteria optimization algorithms.
BARD can analyze and compare plans for differences in assignment and criteria. 
BARD supports heuristic exploration of plans, in order to show trade-offs among redistricting criteria.
}

\details{
\tabular{ll}{
Package: \tab BARD\cr
Type: \tab Package\cr
Version: \tab 1.10 \cr
Date: \tab 2010-4-24 \cr
License: \tab AGPL 3.0 \cr
}

Bard supports several areas of functionality: 
  1. Plan input output. Reading and writing plans in various formats.
  ( \link{readBardImage}, \link{writeBardImage}, \link{readBardCheckpoint}, \link{writeBardCheckpoint},\link{importBardShape}, \link{exportBardShape},\link{readBardMap},\link{writeBardMap} )
  
  2. Initial plan generation. 
  
  Quick heuristics for generating random plans, or plans based on a fixed set of criteria.
  (\link{createRandomPlan}, \link{createRandomPopPlan}, \link{createContiguousPlan}, \link{createKmeansPlan}, \link{createWeightedKmeansPlan}, \link{createGreedyContiguousPlan})
  
  Heuristics for fixing contiguity and unassigned blocks.
(\link{fixContiguityPlan},   \link{fixUnassignedPlan})

  As a quick method for exploration, single districts can be pseudo-sampled.
  (\link{createContiguousDistrict},\link{quickSampleDistricts})
  
  3. Interactive plan editing. Adjust plans interactively, using a mouse.
  (\link{editPlanInteractive},\link{createPlanInteractive})

  4. Plan scoring. Scoring functions for use in plan refinement, profiling, and comparison.
  (\link{calcContiguityScore}, \link{calcReockScore}, 
  \link{calcPACompactScore}, \link{calcLWCompactScore}, \link{calcGroupScore}, \link{calcPopScore}, \link{calcSpatialHolesScore}, \link{calcUnassignedScore}, \link{calcRangeScore}, \link{calcMomentScore}, \link{calcBBCompactScore}, \link{combineDynamicScores},\link{calcSplitScore},\link{calcIneqScore})
  
  5. Plan refinement. Multi-criteria optimization heuristics for refining plans to meet specified goals.
  (\link{refineGreedyPlan}, \link{refineAnnealPlan}, \link{refineGenoudPlan}, \link{refineNelderPlan}, \link{refineGRASPPlan},\link{startBardCluster} ,\link{refineTabuPlan})
  
  6. Plan sampling, profiling and exploration. Generate profiles of plans to explore tradeoffs among redistricting criteria. This can be used in conjunction with \emph{snow} to distribute plan generation across a computing cluster, or with \emph{multicore} to run in parallel across multiple cores on a single system.
  
  (\link{samplePlans}, \link{quickSampleDistricts}, \link{profilePlans}, \link{startBardCluster} )
  
  8. Plan comparison and analysis. Plot, score, and compare plans. Create HTML reports.
  (\link{reportPlans}, \link{scorePlans}, \link{diff.bardPlan}, \link{choroplotPlan},\link{PMPreport})

}

\author{
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/}  

Maintainer:   Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/} 
}

\references{
  Altman \& McDonald, 2004. A computation intensive method for Evaluating Intent in Redistricting. Presented at the Midwest Political Science Associaton Meeting,     Micah Altman, 1997. ``Is Automation the Answer? The Computational Complexity of Automated Redistricting'', Rutgers Computer and Technology Law Journal 23 (1), 81-142
  \url{http://www.hmdc.harvard.edu/micah\_altman/pubpapers.shtml}
  
  Altman, M. 1998. Modeling the Effect of Mandatory District Compactness on Partisan Gerrymanders, \emph{Political Geography} 17:989-1012.
  
C. Cirincione , T.A. Darling, and T.G. O'Rourke. 2000. ``Assessing South Carolina's 1990's Congressional Districting.'' Political Geography 19: 189-211.
  
  Micah Altman and Michael P. McDonald. 2004. A Computation Intensive Method for Detecting Gerrymanders  Paper presented at the annual meeting of the The Midwest Political Science Association, Palmer House Hilton, Chicago, Illinois, Apr 15, 2004. \url{http://www.allacademic.com/meta/p83108\_index.html}

  Micah Altman, Karin Mac Donald, and Michael P. McDonald, 2005. ``From Crayons to Computers: The Evolution of Computer Use in Redistricting'' Social Science Computer Review 23(3): 334-46. 
  
}

\keyword{ spatial }

\seealso{

   \code{\link[spdep]{spdep}} , 
   \code{\link[maptools]{readShapePoly}} ,
   \code{\link[rgenoud]{genoud}} 
}
\examples{
  
    suffolk.map <- importBardShape(
      file.path(system.file("shapefiles", package="BARD"),"suffolk_tracts")
  )
    

  numberdists <- 5
  kplan <- createKmeansPlan(suffolk.map,numberdists)
  rplan <- createRandomPlan(suffolk.map,numberdists)
  rplan2 <- createRandomPopPlan(suffolk.map,numberdists)
  plot(kplan)

reportPlans(plans=list("kmeans"=kplan,"random 1"=rplan,"random pop"=rplan2), doplot=TRUE)

   \dontrun{
   if (require("iplots",quietly=TRUE)) {
	rplan<-editPlanInteractive(rplan,calcPopScore,predvar="POP")
   }
   }

     # district sampling - quick
  
  randomDists<-quickSampleDistricts(10,suffolk.map,numberdists)
      distscores<- scorePlans(randomDists,scoreFUNs=list("LWCompact"=calcLWCompactScore,"PACompact"=calcPACompactScore,"PopScore"=calcPopScore ))
    plot(distscores[2:3])
   
  myScore<-function(plan,...)  {
    return(calcContiguityScore(plan,...))
  }      

#just for quick demonstration -- nelder method not effective

improvedRplan<-refineNelderPlan(plan=rplan2, score.fun=myScore, displaycount=100, historysize=0, dynamicscoring=FALSE, tracelevel=1, maxit=100)

\dontrun{
 # This works better, but will take a while
 improvedRplan<-refineAnnealPlan(plan=rplan2, score.fun=myScore, historysize=0, dynamicscoring=FALSE, tracelevel=1)
}

samples<-samplePlans(kplan, score.fun=myScore, ngenplans=10, gen.fun = "createRandomPlan", refine.fun="refineNelderPlan",refine.args=list(maxit=200,dynamicscoring=TRUE))


profplans<-profilePlans(  list(kplan,rplan), score.fun=calcContiguityScore, addscore.fun=calcPopScore, numevals=2, weight=c(0,.5,1), refine.fun="refineNelderPlan",refine.args=list(maxit=200,dynamicscoring=TRUE) )
 
summary(samples)
plot(summary(samples))
reportPlans(samples)
plot(summary(profplans))

}

\keyword{IO}
\keyword{spatial}
\keyword{optimize}
\keyword{models}
\keyword{distribution}
