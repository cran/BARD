\name{fillHolesPlan}
\alias{fillHolesPlan}
\title{Fill holes in a redistricting plan.}
\description{
This fills holes (unassigned blocks) in a redistricting plan, using selectable methods.
}
\usage{
fillHolesPlan(plan,method=c("random","fixed","closest"), fixed=1)
}


\arguments{
 \item{plan}{input plan}
 \item{method}{Hole filling method to use}
 \item{fixed}{id for ``fixed'' method}

}

\details{
Fixed method assigns all missing blocks to a fixed value. Random method assigns blocks randomly. Closest assigns to a randomly chosen adjoining district (iteratively, if blocks are surrounded by other missing blocks
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
  
}
\keyword{spatial}
