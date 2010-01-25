\name{fillHolesPlan}
\alias{fillHolesPlan}
\alias{fixContiguityPlan}
\alias{fixUnassignedPlan}
\title{Fixes up districting plan.}
\description{
These functions assign unassigned blocks, and join non-contiguous regions.  
}
\usage{
fixUnassignedPlan(plan,method=c("random","fixed","closest"), fixed=1)
fillHolesPlan(plan,method=c("random","fixed","closest"), fixed=1)
fixContiguityPlan(plan,scoreFUN=NULL,...,display=FALSE) 
}


\arguments{
 \item{plan}{input plan}
 \item{method}{Hole filling method to use}
 \item{fixed}{id for ``fixed'' method}
 \item{scoreFUN}{score to minimize when merging non-contiguous portions, if omitted, minimizes number of blocks reallocated}
 \item{\dots}{additional arguments to pass to score function}
 \item{display}{dynamic plotting for demos and teaching}
}

\details{
	\code{fillUnassignedPlan} - Assigns non-assigns plocks . Fixed method assigns all missing blocks to a fixed value. Random method assigns blocks randomly. Closest assigns to a randomly chosen adjoining district (iteratively, if blocks are surrounded by other missing blocks).
(\code{fixHolesPlan} is an alias for fillUnassignedPlan)

\code{fixContiguityPlan} - Evaluates all districts. For each disctict with noncontigous portions, iteratively assigns to an existing contiguous district, greedily minimizing the given score function. 


}

\value{
  Returns a bard plan. 
}

\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah\_altman/} 
 }

 \references{
    Micah Altman, 1997. ``Is Automation the Answer? The Computational Complexity of Automated Redistricting'', Rutgers Computer and Technology Law Journal 23 (1), 81-142
  \url{http://www.hmdc.harvard.edu/micah\_altman/pubpapers.shtml}

  Micah Altman, Karin Mac Donald, and Michael P. McDonald, 2005. ``From Crayons to Computers: The Evolution of Computer Use in Redistricting'' Social Science Computer Review 23(3): 334-46. 
 }

\seealso{ 
    Plan generation algorithms: \code{\link{createRandomPlan}},  \code{\link{createKmeansPlan}}, \code{\link{createContiguousPlan}}, \code{\link{createRandomPopPlan}}, \code{\link{createAssignedPlan}}.
 }
 
\examples{
  suffolk.map <- importBardShape(
      file.path(system.file("shapefiles", package="BARD"),"suffolk_tracts")
  )
  kplan <- createKmeansPlan(suffolk.map,5)
  kplan2<-kplan
  is.na(kplan2[c(1,10,20,100)])<-TRUE
  print(kplan2)
  kplan3 <- fillHolesPlan(kplan2,method="closest")
  
  # create non contiguous plan
  rplan<-createRandomPopPlan(suffolk.map,5)
  
  # fix it
  rplanc<-fixContiguityPlan(rplan,calcPopScore)
  
}
\keyword{spatial}
