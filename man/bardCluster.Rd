\name{startBardCluster}
\alias{startBardCluster}
\alias{stopBardCluster}
\title{Use bard with snow distributed computing clusters.}
\description{
These functions configure a snow computing cluster to be used with bard profiling, sampling, and plan refinement
}


\usage{
  startBardCluster(cl)
  stopBardCluster()
}


\arguments{
\item{cl}{either a cluster returned from \code{snow} or a vector of cluster systems names}
}

\details{
This function attempts to configure a computing cluster for bard. If given an existing \code{snow} cluster, it will use that. If given a vector of  machine names it will use \code{makeCluster} to start a socket-based cluster. (You will be required to enter your login password for these systems, if you have not set up an existing ssh key.)

Plan sampling and profiling, and some plan refinement will automatically use a cluster that has been initialized.

\code{stopBardCluster} stops and disbands the cluster configured through \code{startBardCluster}. It is normally called automatically when the BARD module is unloaded. 

}

\note{
Initialization will fail if attempts to connect to the machines fail, or if BARD cannot be installed and started on these systems (an attempt to install BARD will automatically be made, if BARD is not installed.)
}

\value{
  Returns a logical value indicating successful initialization.
}

\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/} 
 }


\seealso{ 
Plan refinement algorithms \code{\link{refineGenoudPlan}}

Plan sampling: \code{\link{samplePlans}}, \code{\link{profilePlans}}

Cluster computing: [snow:makeCluster]{makeCluster}
 }
 
\examples{
  \dontrun{
	suffolk.map <- importBardShape(file.path(system.file("shapefiles", package="BARD"),"suffolk_tracts"))  
	  numberdists <- 5
  kplan <- createKmeansPlan(suffolk.map,numberdists)
  rplan <- createRandomPlan(suffolk.map,numberdists)
  myScore<-function(plan,...)  {
    return(calcContiguityScore(plan,...))
  }         

# here is where we try to start the cluster!
	startBardCluster(c("localhost","localhost")) 

# this will use the cluster automagically
samples<-samplePlans(kplan, score.fun=myScore, ngenplans=4, gen.fun = "createRandomPlan", refine.fun="refineNelderPlan",refine.args=list(maxit=200,dynamicscoring=TRUE))

profplans<-profilePlans(  list(kplan,rplan), score.fun=calcContiguityScore, addscore.fun=calcPopScore, numevals=2, weight=c(0,.5,1), refine.fun="refineNelderPlan",refine.args=list(maxit=200,dynamicscoring=TRUE) )

}

}

\keyword{ IO }
\keyword{ spatial }
