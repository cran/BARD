\name{refineGreedyPlan}
\alias{refineGreedyPlan}
\alias{refineAnnealPlan}
\alias{refineGenoudPlan}
\alias{refineGreedyPlan}
\alias{refineNelderPlan}
\alias{refineGRASPPlan}
\alias{refineTabuPlan}
\title{Create an initial plan for later refinement}
\description{
These methods create plans starting with random seeds. These plans should
be refined with one of the refinement functions.

}

\usage{
refineGreedyPlan(plan, score.fun, displaycount=NULL, historysize=0, dynamicscoring=FALSE,   tracelevel=1, checkpointCount=0, resume=FALSE)
refineGenoudPlan(plan, score.fun, displaycount = NULL, historysize = 0,
                 dynamicscoring = FALSE, usecluster = TRUE, tracelevel=1)
refineAnnealPlan(plan, score.fun, displaycount = NULL, historysize = 0,
                 dynamicscoring = FALSE,  tracelevel=1, greedyFinish=FALSE)
refineNelderPlan( plan, score.fun, displaycount = NULL, maxit = NULL,
                 historysize = 0, dynamicscoring = FALSE,  tracelevel = 1)
refineTabuPlan(  plan, score.fun, displaycount = NULL, historysize = 0,
                 dynamicscoring = FALSE, tracelevel = 1, tabusize = 100,tabusample=500, checkpointCount=0, resume=FALSE)
refineGRASPPlan (plan, score.fun, samplesize = 50, predvar = NULL,
                 displaycount = NULL, historysize = 0, 
		dynamicscoring = FALSE, usecluster = TRUE, tracelevel = 1)
}

\arguments{
  \item{plan}{plan to be refined, this can be randomly generated through the plan generation functions below, or can be an existing real plan}
  \item{score.fun}{a function that accepts a plan and returns a single score, or a vector of scores, which will be summed to produce the overall plan score. The score function should accept the arguments, plan, lastscore, and changelist. The last two are for incremental scores, and their values may be ignored if the score function does not support incremental score recalculation. See \code{calcPopScore} for an example of a score function.}
  \item{usecluster}{whether to use the defined bard computing cluster, if available. Only refineGenoudPlan, and refineGRASPPlan implements cluster level paralellism at this time. Support for rgenoud clustering with BARD is experimental -- performance is likely to be very bad unless the connections are very fast. }
 \item{displaycount}{This is primarily for demos. It updates a plot of the last plan to be scored (which may not be the current iteration's best scoring candidate) every N iterations}
\item{tracelevel}{Provides a trace of the algorithms progress. For debugging. Currently the legal values are 0 (silence), 1 (minimal information), 2 (more information), 3 (maximum information) }
\item{dynamicscoring}{Makes use of incremental score functions by tracking changes between candidate plans and scoring only the incremental changes. Not implemented for refineGenoudPlan, since multiple candidates are evaluated simulatneously.Use this only if score functions are expensive to compute.}
\item{historysize}{Keep a score history of size n, in order to avoid reevaluating scores for plans already seen. Not currently useful for refineGreedyPlan, since it never revisits plans. Plan digests are used to avoid storing many full plans in memory, however this increases the computation necessary to track the history. Use this when computing a score function is very expensive.}
\item{greedyFinish}{Refine final solution with hill-climbing}
\item{maxit}{Maximum number of iterations before stopping}
\item{predvar}{Population variable for GRASP plan}
\item{samplesize}{Number of samples to use for GRASP}
\item{tabusize}{Size of tabu list}
\item{tabusample}{Number of samples to take at each iteration }
\item{checkpointCount}{Saves checkpoints to global \code{BardCheckPoint} variable, of internal state every Nth iteration, so that function can be resumed}
\item{resume}{Resume interupted optimization, from checkpointed state}
}

\details{
	Greedy -- The greedy algorithm uses hill climbing: at every iteration it compares all possible assignments of single blocks to a spatially contiguous district, and selects the best scoring. Stops when no improvement can be made. This can be interrupted and restarted with the checkpoint and resume arguments. 


	Tabu -- A modification of the greedy method. Samples the assignments of simple blocks. Always takes a move that yields a better solution than the best seen before. Otherwise takes a trade that yields something better than the most recent seen, unless that trade has been used recently and is still on the tabu list. This can be interrupted and restarted with the checkpoint and resume arguments. 
	
	
	Genoud -- uses the \code{\link[rgenoud]{genoud}} implementation of the genetic algorithm. This function tunes the nine genoud operators for the redistricting problem.
	
	
	Anneal -- uses simulated annealing. A plan generation function is provided that generates plans through one-way assignments or two-way exchanges between neighboring blocks of different districts.
	
	
	Nelder -- uses nelder-mead. For demonstration only. Not very effective.
	
	
	GRASP -- GRASP uses greedy randomized adaptive search. Essentially this randomly generates plans via randomPopPlan, and refines using greedy refinement. Greedy refinement is also used on the seed plan. The best of the set is returened. Extremely compute intensive and will make use of clusters if available.

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
  
  Altman, M. 1998. Modeling the Effect of Mandatory District Compactness on Partisan Gerrymanders, \emph{Political Geography} 17:989-1012.
  
  Micah Altman and Michael P. McDonald. 2004. A Computation Intensive Method for Detecting Gerrymanders  Paper presented at the annual meeting of the The Midwest Political Science Association, Palmer House Hilton, Chicago, Illinois, Apr 15, 2004. \url{http://www.allacademic.com/meta/p83108\_index.html}

  Micah Altman, Karin Mac Donald, and Michael P. McDonald, 2005. ``From Crayons to Computers: The Evolution of Computer Use in Redistricting'' Social Science Computer Review 23(3): 334-46. 
}

\seealso{ 
Plan generation algorithms: \code{\link{createRandomPlan}},  \code{\link{createKmeansPlan}}, \code{\link{createContiguousPlan}}, \code{\link{createRandomPopPlan}}, \code{\link{createAssignedPlan}}.

Scoring functions: \code{\link{calcContiguityScore}}
}
 
\examples{


suffolk.map <- importBardShape( file.path(system.file("shapefiles", package="BARD"),"suffolk_tracts"))
numberdists <- 5
rplan <- createRandomPlan(suffolk.map,numberdists)
myScore<-function(plan,...){calcContiguityScore(plan,standardize=FALSE)}
# for example - not very effective
improvedRplan<-refineNelderPlan(plan=rplan,score.fun=myScore,displaycount=50,tracelevel=0, maxit=300)
\dontrun{
	improvedRplan<-refineTabuPlan(plan=rplan,score.fun=myScore)
}
}
\keyword{IO}
\keyword{spatial}
\keyword{optimize}
\keyword{models}
\keyword{distribution}
