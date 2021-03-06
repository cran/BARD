##############################################################################
#
# bard plan refinement
#
# 
#
# The public refineXXXplan methods all accept the following arguments
# (others may also be accepted):
#
#   plan - the starting plan
#   score.fun - the score function to be used,
#       this should accept the following arguments:
#          - plan
#          - 
#          accept return a single score, or a vector of N scores (one for
#          each district) which will 
#   displaycount - whether to provide a demo plot (slow)
#   historysize - size of score history to keep
#   dynamicscoring - whether to track changes and use incremental scoring
#   usecluster - whether it is permissible to use bard cluster 
#               Note: only refineGenoudPlan supports this currently
#   tracelevel - diagnostic traces. Zero means no tracing. 1 means
#       basic progress diagnostics, 2 is more detailed, 3 is verbose 
#
# Internally, a score wrapper is used to support change tracking,
# discreteness constraints, visualization.
#
#############################################################################

#################################
#
# refineGreedyPlan, refineTabuPlan
#
# This uses hill-climbing and tabu search to refine plans. 
#
# Greedy  will always accept the best candidate in the iteration. 
#
# Tabu
#     tabu search algorithm
#     Tabu list contains list of previous used district exchanges
#     Aspiration criterion is beating the best score
#     A sample of the possible exchanges are checked each time
#
# The neighborhood is defined as the set of plans generated by moving
# a single block from one district to a district that borders it.
#
#
# Arguments
#       Arguments as above. And note:
#        -  usecluster is ignored
#                          
# Returns
#    - new bard plan
#
# SEE R HELP FILE  FOR FULL DETAILS
# 
##################################

refineGreedyPlan <- function(plan, score.fun, displaycount=NULL, historysize=0, dynamicscoring=FALSE,   tracelevel=1, checkpointCount=0, resume=FALSE ) {
    retval <- refineGreedyPlanPlus(plan=plan,score.fun=score.fun,displaycount=displaycount,historysize=historysize,
     dynamicscoring=dynamicscoring,tracelevel=tracelevel,tabusize=0, checkpointCount=checkpointCount, resume=resume)
    return(retval)     
}

refineTabuPlan <- function(plan, score.fun, displaycount=NULL, historysize=0, dynamicscoring=FALSE,   tracelevel=1,tabusize=100, tabusample=500, checkpointCount=0, resume=FALSE) {
    retval <- refineGreedyPlanPlus(plan=plan,score.fun=score.fun,displaycount=displaycount,historysize=historysize,
    dynamicscoring=dynamicscoring,tracelevel=tracelevel,tabusize=tabusize, checkpointCount=checkpointCount, resume=resume)
    return(retval)     
}


#################################
#
# refineNelderPlan
#
# This uses nelder-mead to refine plans. It will always accept the best
# candidate in the iteration. 
#
# The neighborhood is defined as the set of plans generated by moving
# a single block from one district to a district that borders it.
#
#
# Arguments
#       Arguments as above. And note:
#        -  usecluster is ignored
#                          
# Returns
#    - new bard plan
#
# SEE R HELP FILE  FOR FULL DETAILS
# 
##################################


refineNelderPlan <- function(plan, score.fun, displaycount=NULL,  maxit=NULL,
 historysize=0, dynamicscoring=FALSE,   tracelevel=1 ) {
  
  control<-list()                                                        
  control$fnscale<-1
  if (tracelevel>1) {
    control$trace=9
  }
  if (is.null(maxit)) { 
    control$maxit<-10*length(plan)
  } else {
    control$maxit <- maxit
  }

                 
  # get plan stuff
  ndists<-attr(plan,"ndists")
  basemap<-basem(plan)

  
  nelderScoreFun <-
        scoreWrapper(score.fun,plan,displaycount,historysize,dynamicscoring,boundCheck=TRUE, tracelevel=tracelevel)

  nelderResult <- optim(plan,nelderScoreFun, method="Nelder-Mead", control=control)
  retval <- convertPar2Plan(nelderResult$par,plan)
  
  if (!is.null(displaycount)) {
    # wrap this in try, in case the window gets closed manually in mid run
    try(plot(retval)) 
  }
  return(retval)     
}

#################################
#
# refineGenoudPlan
#
# This uses an auto-configured genoud() optimization to refine plans.
#
# The neighborhood is defined as the set of plans generated by moving
# a single block from one district to a district that borders it.
#
#
# Arguments
#       Arguments as above. And note:
#        -  usecluster is used if available
#                          
# Returns
#    - new bard plan
#
# SEE R HELP FILE  FOR FULL DETAILS
# 
##################################

refineGenoudPlan <- function(plan, score.fun, displaycount=NULL,  historysize=0, dynamicscoring=FALSE ,   usecluster = TRUE, tracelevel=1 ) {
              
  if(!mrequire("rgenoud", quietly = TRUE, warn.conflicts=FALSE)) {
    stop("Sorry, this requires the rgenoud package. Please install it.")
  }
              
  if (dynamicscoring) {
      warning("Incremental score functions are not likely to increase performance with genetic algorithms")
  }          

  if (tracelevel>1) {
    print.level<-2
  } else {
      print.level<-0
  }
  if (usecluster && !is.null(setBardCluster())) {
    genCluster <- setBardCluster()
    if (is.numeric(genCluster)) {
	# multicore cluster, can't be used
	usecluster<-FALSE
    }
    memMat <- TRUE
    displaycount<-0
  } else {
    genCluster <- FALSE
    memMat <- FALSE
  }                                
               
  # get plan stuff
  ndists<-attr(plan,"ndists")
    basemap<-basem(plan)
  
  
  genoudScoreFun <- 
    scoreWrapper(score.fun,plan,displaycount,historysize,
      dynamicscoring,boundCheck=F, tracelevel=tracelevel)


  # adjust genoud params to plan
  # P3 = 0 because boundary mutation doesn't make sense for district ID's
  # P7 = 0 -- whole non-uniform mutations does not make sesne for this case
  # P8, P4 , might make sense , needs experi mentation
  
  max(pop.size<-sqrt(length(plan))*100,10000)
  max.generations<-length(plan)
  wait.generations<-sqrt(length(plan))
  
  
  genoudResult <- genoud( fn = genoudScoreFun,  nvars=length(plan),  
    pop.size=pop.size, hard.generation.limit=FALSE,
    max.generations=max.generations,
    wait.generation=wait.generations,
    MemoryMatrix=memMat, cluster=genCluster, 
    P3=0, P7=0,
    P4=0, P8=0,
    Domains=t(replicate(length(plan),c(1,ndists))),
    solution.tolerance=0.001, boundary.enforcement=2,
    print.level=print.level,
    data.type.int=T,  starting.values=c(as.double(plan)) )
  retval <- convertPar2Plan(genoudResult$par,plan)
  
  if (!is.null(displaycount)) {
    # wrap this in try, in case the window gets closed manually in mid run
    try(plot(retval)) 
  }
  return(retval)     
}

#################################
#
# refineAnnealPlan
#
# This uses simulated to refine plans. 
#
# The temperature and other parameters are auto-tuned.
#
# The generation function samples from a neighborhood of
# -  single moves of blocks from one district to an adjoining district
# - switches of pairs of adjoining blocks in different districts 
#
# The neighborhood is defined as the set of plans generated by moving
# a single block from one district to a district that borders it.
#
#
# Arguments
#       Arguments as above. And note:
#        -  usecluster is ignored
#                          
# Returns
#    - new bard plan
#
# SEE R HELP FILE  FOR FULL DETAILS
# 
##################################

refineAnnealPlan <- function(plan, score.fun, 
     displaycount=NULL, 
     historysize=0, dynamicscoring=FALSE,
     tracelevel=1, checkpointCount=0, resume=FALSE, greedyFinish=FALSE, doquench = FALSE, doReanneal=!doquench, ... ) {
               
  
  # get plan stuff
  ndists<-attr(plan,"ndists")
    basemap<-basem(plan)
  
  canGen <- function(par) {
    legal <- convertPar2Plan(par,plan,boundCheck=F)
    candidateBlocks <- locallyExchangeableBlocks(legal)
    if (length(candidateBlocks)==0) {
        print("Annealing: out of candidates.")     # DEBUGGING ONLY
        return(par)
    }
    newpar<-par
    ex1<-sample(candidateBlocks,1)
    
    nex <- neighbors(basemap$nb,ex1)
    nex2 <- nex[which(par[nex]!=par[ex1])]
    ex2 <- sample(nex2,1)
    newpar[ex1]<-newpar[ex2]
    if (runif(1)>.5) {
      newpar[ex2]<-par[ex1]
    }
    return(newpar)
  }
  

                                                            
  annealScoreFun <-  scoreWrapper(score.fun,plan,displaycount,historysize,dynamicscoring,
        boundCheck=FALSE, tracelevel=max(0,tracelevel-2))
  
        
  control<-list(); control$fnscale<-1; control$trace<-tracelevel; control$REPORT<-50 
 
  if (doquench) {
  	  annealResult <-quench(plan,annealScoreFun,gr=canGen,control=control,checkpointCount=checkpointCount, resume=resume,...)
  } else if (!doReanneal) {
  	  annealResult <-reAnneal(plan,annealScoreFun,gr=canGen,control=control,reannealCount=0,checkpointCount=checkpointCount, resume=resume,...)
  } else {
  	  annealResult <-reAnneal(plan,annealScoreFun,gr=canGen,control=control,checkpointCount=checkpointCount, resume=resume,...)
  } 
  retval <- convertPar2Plan(annealResult$par,plan,boundCheck=F)

  if (greedyFinish) {
    print("Anneal done, finishing up with hill climbing...")
    displaycount<-ceiling(displaycount/100)
    retval <-refineGreedyPlan(retval, score.fun, displaycount=displaycount, 
       dynamicscoring=dynamicscoring,  tracelevel=tracelevel)
  }
  

  if (!is.null(displaycount)) {
    plot(retval)
  }
  
  return(retval)     
}



##############################################################################
#           INTERNAL MODULE FUNCTIONS -- DO NOT EXPORT
###############################################################################


#
# Workhorse for Greedy and Tabu
#


refineGreedyPlanPlus <- function(plan, score.fun, displaycount=NULL, historysize=0,
	tabusize =0, dynamicscoring=FALSE,   tracelevel=1,tabusample=500,
	checkpointCount=0, resume=FALSE) {
                                                          
  if (resume) {
      checkpoint.env<-get("BardCheckPoint",envir=.GlobalEnv)
      lslist=setdiff(ls(checkpoint.env),c("checkpoint.env","resume","checkpointCount"))
      for (enitem in lslist) {
	 assign(enitem,get(enitem,envir=checkpoint.env))
      }   
  }
  
  if (checkpointCount>0 && !resume) {
        checkpoint.env<-new.env()
  	assign("BardCheckPoint",checkpoint.env,envir=.GlobalEnv)
  }
  
  
  if (!resume) {
   # no score history -- greedy plan generation doesn't revisit plans
   
   if (tabusize==0) {
	historysize<-0
   } else {
	tabulist <- matrix(nrow=tabusize,ncol=2)
   }
            
   # get plan stuff
   ndists<-attr(plan,"ndists")
   basemap<-basem(plan)
   nb<-basemap$nb
  
   # tracking vars
   bestScore<-prevscore<-score.fun(plan)
   mitercount<-0
   lastplan<-plan
   lastplotted<-plan
  
   if (!is.null(displaycount)) {
      try(plot(plan))
    }
  
   # helper functions
   candpairs<-function(i) {
     candres<-as.matrix(expand.grid(i,neighbors(nb,i)))
	 # later for switches eliminate duplicates
	 # candres<-t(apply(candres,1,function(x)if(x[1]>x[2]){c(x[1],x[2])}else{c(x[2],x[1])}))
	 # candres<-unique(candres)
	 return(candres)
   }
  
   
   # tricky -- displaycount in scorewrapper set to NULL, since we display inside
   # the repeat loop, adjust tracelevel
   
   greedyScore <-
        scoreWrapper(score.fun,plan,
        displaycount=NULL,historysize=historysize,dynamicscoring=dynamicscoring,
        boundCheck=FALSE, tracelevel=max(0,tracelevel-1))
   
   # scores a single exchange from the current plan
   scoreMove<-function(spair,curplan,returnScore=TRUE,
    lastscore=NULL ) {
    home<-spair[1]
    newid<-spair[2]
    testplan<-curplan
    testplan[newid]<-plan[home]
    changelist<-matrix(ncol=2, c(newid,curplan[newid]))
    
    if (returnScore) {
      return(greedyScore(testplan,changelist=changelist,lastscore=lastscore))
    } else {
      return(testplan)
    }
   }

   maxiter <- length(plan)
   iterSinceImproved<-0
   mitercount<-0
   bestPar<-NULL
  } # end if resume
  
  repeat {
     mitercount <- mitercount +1
	 iterSinceImproved<-iterSinceImproved +1
	 
	 # new candidate  is best of single one way moves
     lb<-locallyExchangeableBlocks(lastplan) 
	 ltmp<-lapply(lb,candpairs)
     lpairs<-cbind( unlist(lapply(ltmp,function(x)x[,1])),
         unlist(lapply(ltmp,function(x)x[,2]))) 
	
	 # Later -- for 2 d exchanges dedup
	 #lpairs<-t(apply(lpairs,1,function(x)if(x[1]>x[2]){c(x[1],x[2])}else{c(x[2],x[1])}))
	 #lpairs<-unique(lpairs)
	 
	 # single switch only
	 dupid<-duplicated(cbind(lpairs[,1],lastplan[lpairs[,2]]))
	 lpairs<-lpairs[!dupid,,drop=F]
	 
     if (tracelevel) {
        print(paste("MAJOR ITERATION ",mitercount,": total ",dim(lpairs)[1],"candidates",Sys.time())) 
        flush.console()
     }   
	if (tabusize>0) {
			samplesize <- min(dim(lpairs)[1],tabusample)
			ind<-sample(dim(lpairs)[1],samplesize)
			lpairs<-lpairs[ind,,drop=F]
				if (tracelevel) {
					print(paste("Evaluating ",dim(lpairs)[1],"candidates",Sys.time())) 
					flush.console()
				} 
	 }
  
     if (dynamicscoring) {
       lpairsScores<- apply(lpairs,1,
        function(x)scoreMove(x,curplan=lastplan,lastscore=prevscore))
     } else {
        lpairsScores<- apply(lpairs,1,function(x)scoreMove(x,curplan=lastplan))
     }
      
     bestswitch<-which.min(lpairsScores)
     candidatePar<-scoreMove(lpairs[bestswitch,],curplan=lastplan,returnScore=F)
     
	 curscore <- lpairsScores[bestswitch]
     if (sum(curscore)>=sum(bestScore)) {
		# non-improvement -- what to do...

	  if (tabusize==0) {
		#browser()
		break
		# GREEDY, local opt
		# finished ...
	  } else if (iterSinceImproved > maxiter) {
		break
		# TABU max iterations
		# finished...
	  } else if (any(apply(na.omit(tabulist),1,function(x)all(x==lpairs[bestswitch,])))) {
		# TABU current solution on tabu list
		
		if (tracelevel) {
			print(paste("Current solution is TABUed, iterSinceImproved:", iterSinceImproved)) 
			flush.console()
		}  
		
		# exclude other tabu solutions
		tmpm<- rbind(lpairs,unique(na.omit(tabulist)))
		dups<-duplicated(tmpm,fromLast=TRUE)[1:dim(lpairs)[1]]
		
		# any solutions left? 
		if (sum(dups)>0) {
			legalscores<-lpairsScores[!dups]
			legalpairs<-lpairs[!dups,,drop=F]
			bestlegal<-which.min(legalscores)
			candidatePar<-scoreMove(legalpairs[bestlegal,,drop=F],curplan=lastplan,returnScore=F)
			tabulist [mitercount %% tabusize,] <- legalpairs[bestlegal,,drop=F]

		} else {
			if (tracelevel) {
				print(paste("All tabued")) 
				flush.console()
			}  
		}
		
	  }
     } else {
		# improvement -- meets greedy and tabu aspiration criteria
		iterSinceImproved=0
		bestScore<-curscore
		bestPar <- candidatePar
		if (tabusize>0) {
			tabulist [mitercount %% tabusize,] <- lpairs[bestswitch,]
		}
	 }


     # eval score
     score<-score.fun(candidatePar)
     
     # post tracking
     prevscore<-score
     lastplan<-candidatePar
     
     if (tracelevel) {
        print(paste("MAJOR ITERATION:  Best Score",bestScore, "current score",sum(score),"iteration",mitercount,Sys.time())) 
        flush.console()
     }           
     

     if (!is.null(displaycount)) {
      
        if ((mitercount %% displaycount) ==0) {
           updatePlot(candidatePar,lastplotted,score,mitercount)
           lastplotted<-candidatePar  
        }
      }
     if (checkpointCount>0 && (mitercount %% checkpointCount ==0)) {
       
       	for (enitem in ls()) {
		assign(enitem,get(enitem),envir=checkpoint.env)
	}
     
     }
  }
  
  retval<-bestPar
  if (!is.null(displaycount)) {
    # wrap this in try, in case the window gets closed manually in mid run
    try(plot(retval)) 
  }
  return(retval)     
}



#  ScoreWrapper Functions
# 
#  The workhorse is scoreWrapper, updatePlot() and convertPar2Plan() are support 
# functions called by  scoreWrapper
#



#################################
#
# scoreWrapper
#
# This is a workhorse function used by most of the refineXXXplan functions to
# prepare a bard score function for use with a general purpose optimizer such 
# as optim()
#
# It uses a closure to store tracking variables for iteration counts,
# score histores, etc. It also performs plot updating for demos,
# boundschecking for optimizers that work in continuous space.
#
#
# Arguments
#    See the description of refineXXXplan above
#   
#   Additionally:
#
#  - FUN - original score function (score.fun)
#  - boundCheck - whether to perform boundschecking on input parameter   
#                          
# Returns
#    - new score function
# 
#################################
  
scoreWrapper<-function(FUN,plan,
  displaycount,historysize,dynamicscoring,
  boundCheck=TRUE,tracelevel=0)  {
    #
    # Setup tracking variables in closure
    #
    
  # Tricky Stuff -- this contains lexical vars updated by nested functions
  #                 when the nested score function is called by the optimizers
  #                 this allows us to update plots, keep track of changes
  #                 for incremental scoring, and keep a score history
  #
  # itercount - iteration count for plot updates
  # lastplan - last plan seen for purposes of generating a list of changes
  # history - history of previous function evaluations  
     DEBUG<-FALSE
    
    # change tracking
    lastplotted<-integer(length(plan))
    lastplan<-integer(length(plan))
    itercount<-0
    scorehistory <- vector(historysize,mode="list")
    changelist<-NULL
    savedscore<-NULL
    
    # check dynamic change support
    if (dynamicscoring) {
      funformals<-names(formals(FUN))
      if (!("..." %in% funformals) 
      && !(all(c("lastscore","changelist") %in% funformals) ))       {
        warning("score function does not appear to support dynamic scoring")
        dynamicscoring<-FALSE
      }
      savedscore<-FUN(plan)
      lastplan<-plan
    }
    
    # plotting for demos
    if (!is.null(displaycount)) {
      try(plot(plan))
    }
    
    # build wrapped score function
    wrappedScoreFun <- function(candidatePar,lastscore=NULL,changelist=NULL) {
      # prepare for Function run
     
      itercount<<-itercount+1
      
      # check history
      score<-NULL
      if (historysize>0) {
        plandig <- digest(candidatePar)
        score <- scorehistory[[plandig]]
        if (DEBUG) {
          print(paste("history",plandig,score))
          flush.console()
        }
      }
      if (is.null(score)) {
        candidatePar<-convertPar2Plan(candidatePar,plan,boundCheck)
        # track blocks that changed
        if (dynamicscoring && is.null(changelist)) {
            changelist<-which(lastplan!=candidatePar)
            if (length(changelist)>0) {
              changelist<-cbind(changelist,lastplan[changelist])
            }
            if (DEBUG) {
              print(paste("dynamic",savedscore,"changes",paste(collapse=" ",changelist)))
                flush.console()
            }
        } 
      
        # run score function
       if (dynamicscoring) {
          if (is.null(lastscore)) {
            lastscore<-savedscore
          }
      
          if (length(changelist)==0) {
            score<-lastscore
          } else {
            score<-FUN(candidatePar,lastscore=lastscore,changelist=changelist)
            
          }
        } else {
          score<-FUN(candidatePar)
        }
        #update tracking
        if (historysize>0) {
          names(scorehistory)[itercount%%historysize]<<-plandig
          scorehistory[itercount%%historysize]<<-score
        }
      }
    
      if ((tracelevel>1) || ((tracelevel>0) && ((itercount %% 100) ==0))) {
        print(paste("Score",paste(score,collapse=" "),"iteration",itercount,Sys.time())) 
        flush.console()
      }
      if (!is.null(displaycount)) {
        if ((itercount %% displaycount) ==0) {
            # (re)convert, in case the score was taken from a history list
            candidatePar<-convertPar2Plan(candidatePar,plan,boundCheck)
            
            updatePlot(candidatePar,lastplotted,score,itercount)
            lastplotted<<-candidatePar
        }
      }
      if (dynamicscoring) {
         lastplan<<-candidatePar
         savedscore<<-score
      }
      return(sum(score)) 
    }
    return(wrappedScoreFun)
  }

#################################
#
# updatePlot
#
# For demo's, updates a plot of the current score being evaluted
#
# Arguments
#
#  - plan  -  plan to be plotted
#  - oldplan - previously plotted plan, if known
#  - score - current plan score
#  - itercount - iteration count
#                          
# Returns
#    - NULL
# 
#################################

updatePlot<-function(plan,oldplan=NULL,score=0,itercount=0,refreshcount=500,skipplotcount=1,printit=T
, sleepTime=.05) {
  if (is.null(oldplan)) {
    changed<-1:length(plan)
  } else {
    changed <- which(plan!=oldplan)
  }

  if ((length(changed)>0) && (!itercount %% skipplotcount)) {
    
    if ((skipplotcount<2) && itercount %% refreshcount) {
      # WARNING: newPlot FALSE creates memory leak because of add flag 
      #          in plot.polylist
      res <- try(plot(plan,changed=changed,newPlot=FALSE),silent=TRUE)
       Sys.sleep(sleepTime)


    } else {
    
       # use try, in case window got closed manually
       #try(dev.off(), silent=TRUE)
       res <- try(plot(plan,newPlot=TRUE),silent=TRUE)
       Sys.sleep(sleepTime)
       
    }
    
    if (inherits(res,"try-error")) {
       try(plot(plan,newPlot=TRUE),silent=TRUE)
    }
    
    } else {
    }
  if (printit>0) {
      print(paste("Iterations:",itercount,"(",paste(score,collapse=" "),")*", Sys.time()) )
      flush.console()


  }

  return(NULL)
}

#################################
#
# convertPar2Plan
#
# Most optimization function return numeric character vectors.
# This puts them back into bardPlan form in order to evaluate the scores
#
# Arguments
#
#  - candidatePar  -  optimization vector
#  - plan - original plan
#  - boundCheck - for optimizers that do not respect discreteness and
#                 district ID contraints. Forces par to bounds
#  - itercount - iteration count
#                          
# Returns
#    - new bardplan
# 
#################################

convertPar2Plan <- function(candidatePar,plan,boundCheck=TRUE) {
      ndists<-attr(plan,"ndists")
      
      if (boundCheck) {
         candidatePar<-as.integer(pmin(pmax(1,round(candidatePar)),ndists))
      } else if (!is.integer(candidatePar)) {
         candidatePar<-as.integer(candidatePar) 
      }
        
      attributes(candidatePar)<-attributes(plan)
      return(candidatePar)
}

       
####################
# Misc functions
####################
                         


#################################
#
# locallyExchangeableBlocks
#
# Returns list of blocks in a plan that border blocks in 
# a differnt district
#
# Arguments
#
#  - plan  -  candidate plan
#                          
# Returns
#    - list of blockid's
# 
#################################


locallyExchangeableBlocks<-local({	
oldplan<-NULL
oldcand<-NULL
TS<-NULL
lebInner<- function(plan) {
	nb<-basem(plan)$nb

	if(!is.null(TS) && (TS==basem(plan)$timestamp)) {
		checkset <-which(plan!=oldplan)
		if (length(checkset)>0) {
			checkset<-unique(c(checkset,neighbors(nb,checkset)))
		}
		cand<-oldcand
	} else {
		oldplan<<-NULL
	}

	if (is.null(oldplan)) {
		checkset<-1:length(plan)
		cand<-logical(length(plan))
		TS<<-basem(plan)$timestamp
	}
	
	# filter unassigned blocks -- looking for neighbors of assigned blocks
	checkset<-which(plan[checkset]>0)
	
	if (length(checkset)>0) {
		checkval <- sapply(checkset,function(x){ nbb <- neighbors(nb,x); px<-plan[x]; any(plan[nbb]!=px)},simplify=TRUE)
		if (length(checkval)>0) {
			cand[checkset]<-checkval
		}
	}
	oldcand<<-cand		
	oldplan<<-plan

	return(which(cand))
}	
})
	

####################
# Test functions - Used for development testing, not used in running production code.
####################

quench<-function(par, fn, gr = NULL, ... , control=list(), maxTime = 3600) {
	control$temp=0
	reAnneal (par=par,fn=fn,gr=gr,...,control=control,maxTime=maxTime,
			reannealCount=0, pmiss=.05 )
}

reAnneal <-function( par, fn, gr = NULL, ... , control=list(),
       maxTime = NULL, pmiss=.01, pgood=.001, maxOptFactor=1000,
       minDeltaFactor = .01, minConvergenceTemp=MINSTARTTEMP/exp(3),
       reannealCount=1,
       checkpointCount=0, resume=F,
       itblock = NULL,   MAXTEMP=10000,STARTTEMP=NULL,MINSTARTTEMP=2) 
       {
	
	MINITER <- 5000; MAXITER<-10^7; PARITERSCALE<-10 ; TMAXMIN<-10
	
	if (is.null(maxTime)) {
		maxTime<-3600
	}
	
	if (reannealCount<1) {
		reannealCount <- .Machine$integer.max
	}
		
	# set up control parameters 
	parscale <- control$parscale ; if(is.null(parscale))parscale <- replicate(length(par),1)
	fnscale  <- control$fnscale ; if(is.null(fnscale))fnscale <-1
	if(is.null(trace)){trace<-0} else {trace<-control$trace}
	abstol <- control$abstol ; if(is.null(abstol))abstol<-sqrt(max(.Machine$double.eps,.Machine$double.neg.eps))
	reltol <- control$reltol ; if(is.null(reltol))reltol<-(.Machine$double.eps)^(1/4)
	maxitTotal <- control$maxit ; if (is.null(maxitTotal))maxitTotal<- max(min(MINITER,(round(PARITERSCALE*length(par))^2)),MAXITER)
	tmax <-control$tmax ; if (is.null(tmax))tmax<-(max(TMAXMIN,round(length(par)^(1/3))))
	if (trace > 1) {
		basecontrol<-c(control[intersect(c("trace","REPORT") ,names(control))])
	} else {
		basecontrol<-list()
	}
	scalecontrol<-c(parscale=list(parscale),fnscale=list(fnscale),tmax=tmax )
	
	
	# temp based on fnscale value
	res <- optim(par=par,fn=fn,gr=gr,...,method="SANN",control=c(scalecontrol,maxit=0))
	
	# debugging
			
	if (!is.null(STARTTEMP)) {
		startTemp<-STARTTEMP
	} else if (!is.null(control$temp)) {
		startTemp<-control$temp
	} else {
		#startTemp <- abs(fnscale * res$value * abs(reltol)^(1/2) /log(.1) * log(maxitTotal))
		startTemp<-initialTemp(maxIterations=maxitTotal, minDelta = abs(res$value*fnscale*minDeltaFactor), maxPacceptance = .1)
		startTemp<-max(MINSTARTTEMP,startTemp)
	} 
	maxTemp<-max(MAXTEMP,startTemp*10)


  


	# reannealling loop
	#
	startTime<-Sys.time()
	convergence <- 1
	curit <- 0
	itattemp<-0
	convergenceCount<-0
	if (is.null(itblock)) {
		#itblock<-max(100,round(maxitTotal/1000))
		itblock<-nsamples(pmiss=pmiss,pgood=pgood)
	}
	curtemp<-startTemp
	lastGoodTemp<-startTemp
	
	if (trace) {
		print(match.call()) 
		print(control)
		cat(paste("startTemp maxTemp itblock",startTemp,maxTemp,itblock,"\n"))
	}
	
	### CHECKPOINT RESUME LOGIC   -- May Reset the settings above###
	  if (resume) {
	  	  checkpoint.env<-get("BardCheckPoint",envir=.GlobalEnv)
	  	  lslist=setdiff(ls(checkpoint.env),c("checkpoint.env","resume","checkpointCount"))
	  	  for (enitem in lslist) {
	  	  	  assign(enitem,get(enitem,envir=checkpoint.env))
	  	  }   
	  }
  
	  if (checkpointCount>0 && !resume) {
	  	  checkpoint.env<-new.env()
	  	  assign("BardCheckPoint",checkpoint.env,envir=.GlobalEnv)
	  }
	
	repeat {
		# Save Checkpoint state
		  if (checkpointCount>0) {  
		  	  for (enitem in ls()) {
		  	  	  assign(enitem,get(enitem),envir=checkpoint.env)
			}
		   }
		
		
		#
		# setup and run anealing
		#
		curtemp <- annealTempDecay(startTemp,tmax,itattemp+1)
		if ((!(convergenceCount+1)%%reannealCount && (convergenceCount >0))) {
			itattemp<-0
			startTemp<- min(lastGoodTemp*exp(reannealCount+1), maxTemp)
			if (trace) {
				cat(paste("Reannealing... last, cur, new:",lastGoodTemp,curtemp,startTemp,"\n"))
			}
			lastGoodTemp<-startTemp
		}

		curtemp <- annealTempDecay(startTemp,tmax,itattemp+1)
		if (trace) {
			cat(paste( res$value," (",curit,",",itattemp,round(curtemp,digits=2),") -- value (iteration, iteration at curr temp, temperature)\n"))
		}
		lastRes<-res
		newcontrol = c(basecontrol,scalecontrol,maxit=itblock,temp=curtemp)
		res<-optim(par=lastRes$par,fn=fn,gr=gr,...,method="SANN",control=newcontrol)
		curit <- curit + itblock
		itattemp<-itattemp+itblock
		
		#
		#  evaluate stopping & reannealing criteria
		#
		
		convergedLast<-FALSE
		if (abs(res$value - lastRes$value)<abstol) {
			convergence <- 0
			convergencemsg<-"abstol"
			convergenceCount <-convergenceCount+1
			if (trace) {
				cat(paste("No improvement, convergence count: ",convergenceCount,"\n"))
			}
		} else if (abs(res$value/lastRes$value)<reltol) {
			convergence <- 0
			convergencemsg<-"reltol"
			convergenceCount <- convergenceCount+1
			if (trace) {
				cat(paste("No improvement, convergence count: ",convergenceCount,"\n"))
			}
		}  else {
			convergenceCount <- 0
			lastGoodTemp<-curtemp
		}
		if ( (convergenceCount>0) && ((reannealCount==.Machine$integer.max) || (lastGoodTemp>=MAXTEMP))) {
			break
		}	
		if (curit>maxitTotal) {
			convergence <-1
			convergencemsg<-"maxit"
			break
		}
		if ((Sys.time()-startTime)>maxTime) {
			convergence <-2
			convergencemsg<-"maxtime"
			break
		}
	}
	
	#
	# return results
	#
	
	result<-res
	result$counts[1]<-curit
	result$convergence<-convergence # true convergence code
	result$convergencemsg<-convergencemsg
	if (trace) {
		cat(paste("reAnneal exiting: ",convergencemsg,"\n"))
	}
	return(result)
}


# Rationale for cooling schedule
# 
# At the end, the cooling temperature should be low enough that the
# probability of accepting a relatively bad solution is low:
#
# Note that probAcceptance =exp(deltaCandidate/Temp)
# so at end of iterations, temp must be:
#
# finalTemp = minDelta / log(maxPacceptance)
#
# Also note:
# 
# finalTemp = temp_0/log(iterations)
# temp_0 = finalTemp * log(iterations)
# --> temp_0 = minDelta/log(maxPacceptance)*log(maxIterations)


initialTemp <- function(maxIterations=10^7, minDelta = sqrt(.Machine$double.eps), maxPacceptance = .1) {
		
	res <- abs(minDelta/log(maxPacceptance) * log (maxIterations))
	return(res)
}

annealTempDecay<-function(temp,tmax,curit) {

    curtemp <-  
	temp / log(((curit-1) %/% tmax)*tmax + exp(1))
    return(curtemp)
}



# number of samples to draw for a probability of missing an improvement, where improvements are less than X% of the entire space

nsamples<-function(pmiss=.01,pgood=.001) {
	# pmiss = (1-pgood)^n
	
	res <- round(log(pmiss)/log(1-pgood))
	return(res)
}
