\name{profilePlans}
\alias{profilePlans}
\alias{samplePlans}
\alias{quickSampleDistricts}
\title{Profile or pseudo-sample redistricting plans based on score criteria.}
\description{
These functions creates sets of redistricting plans to show how redistricting criteria affect plan outcoumes
}
\usage{
profilePlans( seedplans, score.fun, ngenplans = 0, 
	gen.fun = "createRandomPlan", gen.args = list(), 
	refine.fun = "refineAnnealPlan", refine.args = list(), 
	addscore.fun = NULL, 
	weight.fun = function(score1, score2, weight)
                 { sum(score1 + weight * score2)}, weight = seq(0, 1,
        length.out = 10, ), numevals = 10, tracelevel = 1,
	usecluster=TRUE)
  
samplePlans(seedplans, score.fun = calcContiguityScore, 
	ngenplans = 24, gen.fun = "createRandomPlan", gen.args = list(),
                 refine.fun = "refineAnnealPlan", refine.args = list(),
                 tracelevel = 1,usecluster=TRUE)
                 
quickSampleDistricts(ngenplans, basemap, ndists, distFUN=createContiguousDistrict, ...)

}


\arguments{
\item{seedplans}{initial plans to be used as seeds}
\item{score.fun}{base score function}
\item{ngenplans}{number of additional plans to generate}
\item{gen.fun}{function for generating additional plans}
\item{gen.args}{a list of additional arguments to gen.fun}
\item{refine.fun}{function for plan refinement}
\item{refine.args}{a list of additional arguments to planRefineFun}
\item{addscore.fun}{additional score component}
\item{weight.fun}{function to generate weighted score}
\item{weight}{vector of weight} 
\item{numevals}{number of evaluations per plan at each point}
\item{tracelevel}{indicates desired level of printed tracing of optimization, 0 = no printing, higher levels give more detail. Nine is maximum}
\item{usecluster}{use the bard cluster for computations if available}
 \item{basemap}{Bard basemap from which to create plan}
 \item{ndists}{number of districts to create} 
 \item{distFUN}{district generating function to use}
 \item{\dots}{arguments to pass to distFUN}
}

\details{
\code{samplePlans} generates a set of plans, adds these to the seed plans given, and refines them based on the score function, to create a pseudo-sample of plans optimizing a particular score.\code{profilePlans} generates a set of plans pseudo-sampled using a two-part score function, where the weight of the second part is repeatedly.

\code{quickSampleDistricts} is a quick-and-dirty way of sampling a contiguous, equipopulous district (which may or may not be part of a plan containing only contigous districts) These sample districts may be scored using the scoring functions, but may not be used with plan refinement functions at present. 
}

\note{
	\code{samplePlans} and \code{profilePlans}  can be very compute intensive. If a compute cluster is configured, these functions will automtically distribute the computing load across the cluster. See \code{startBardCluster}.
}

\value{
  Returns a list of bard plans.
}

\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/} 
 }

 \references{
  o
  Micah Altman, 1997. ``Is Automation the Answer? The Computational Complexity of Automated Redistricting'', Rutgers Computer and Technology Law Journal 23 (1), 81-142
  \url{http://www.hmdc.harvard.edu/micah\_altman/pubpapers.shtml}
  
  Altman, M. 1998. Modeling the Effect of Mandatory District Compactness on Partisan Gerrymanders, \emph{Political Geography} 17:989-1012.
  
C. Cirincione , T.A. Darling, and T.G. O'Rourke. 2000. ``Assessing South Carolina's 1990's Congressional Districting.'' Political Geography 19: 189-211.
  
  Micah Altman and Michael P. McDonald. 2004. A Computation Intensive Method for Detecting Gerrymanders  Paper presented at the annual meeting of the The Midwest Political Science Association, Palmer House Hilton, Chicago, Illinois, Apr 15, 2004. \url{http://www.allacademic.com/meta/p83108\_index.html}

  Micah Altman, Karin Mac Donald, and Michael P. McDonald, 2005. ``From Crayons to Computers: The Evolution of Computer Use in Redistricting'' Social Science Computer Review 23(3): 334-46. 
 }

\seealso{ 
Plan refinement alogrithms \code{\link{refineGreedyPlan}},  \code{\link{refineAnnealPlan}}, \code{\link{refineGenoudPlan}}, \code{\link{refineNelderPlan}}

Cluster computing: \code{\link{startBardCluster}}
 }
 
\examples{
 data(suffolk.map)
 
  numberdists <- 5
  
  # district sampling - quick
  
  randomDists<-quickSampleDistricts(10,suffolk.map,numberdists)
      distscores<- scorePlans(randomDists,scoreFUNs=list("LWCompact"=calcLWCompactScore,"PACompact"=calcPACompactScore,"PopScore"=calcPopScore ))
    plot(distscores[2:3])

  
  # plan sampling and refinement -- compute intensive
  
  kplan <- createKmeansPlan(suffolk.map,numberdists)
  rplan <- createRandomPlan(suffolk.map,numberdists)
  rplan2 <- createRandomPopPlan(suffolk.map,numberdists)

  myScore<-function(plan,...)  {
    return(calcContiguityScore(plan,...))
  }   

 samples<-samplePlans(kplan, score.fun=myScore, ngenplans=20, gen.fun = "createRandomPlan", refine.fun="refineNelderPlan",refine.args=list(maxit=200,dynamicscoring=TRUE))


profplans<-profilePlans(  list(kplan,rplan), score.fun=calcContiguityScore, addscore.fun=calcPopScore, numevals=2, weight=c(0,.5,1), refine.fun="refineNelderPlan",refine.args=list(maxit=200,dynamicscoring=TRUE) )




 
summary(samples)
plot(summary(samples))
reportPlans(samples)
plot(summary(profplans))
}

\keyword{ IO }
\keyword{ spatial }
