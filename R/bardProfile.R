##############################################################################
#
# bard plan profiling
#
# 
# This module contains methods to profile across a set of  plans.  
#
##############################################################################


#################################
#
# profilePlans
#
# This generates a set of plans pseudo-sampled using the same
# score function components, but with  a aggregate score function
# based on reweighting sum(score.fun,addScoreFun).
#
# The function will automatically use a computing cluster for
# the evaluation if one has been started via startBardCluster()
#
#   seedplans - initial plans to be used as seeds
#   score.fun - base score functiion
#   ngenplans - number of additional plans to generate
#   gen.fun - function for generating additional plans
#   gen.args - a list of additional arguments to gen.fun
#   refine.fun - function for plan refinement
#   planRefineArgs - a list of additional arguments to refine.fun
#   assScoreFun - additional score component
#   weight.fun = sunction to generate weighted score
#   scoreWeights - vector of scoreWeights  
#   numEvals - number of evaluations per plan at each point
#
##################################

profilePlans<-function(
  seedplans,
  score.fun, ngenplans=0,
  gen.fun = "createRandomPlan", gen.args=list(),
  refine.fun = "refineAnnealPlan", refine.args=list(),
  addscore.fun=NULL,  
  weight.fun = function(score1,score2,weight){sum(score1+weight*score2)},
  weight = seq(0,1,length.out=10,),
  numevals =10,
  tracelevel=1,
  usecluster=TRUE
  
   ) {
    
   
  if (!is.list(seedplans)) {
    seedplans<-list(seedplans)
  }
  
  if (!checkPlans(seedplans)) {
    return(NULL)
   }

   basemap<-basem(seedplans[[1]])
   ndists<-attributes(seedplans[[1]])$ndists
 
   gen.args$ndists<-ndists
   gen.args$basemap<-as.name("basemap")
   if ("usecluster" %in% names(formals(eval(as.name(refine.fun))))) { 
   	refine.args$usecluster<-FALSE
   }

  usingcluster<-FALSE
  cl<-setBardCluster()
  if (!is.null(cl) && usecluster) {
	if (tracelevel>0) {
		print("Will use cluster for sampling, suppressing graphics, some tracing unavailable")
		flush.console()
	}
	refine.args$displaycount<-NULL
	refine.args$tracelevel<-0
  	usingcluster<-TRUE
  }

   
 mygenplan<-function(x) {
  if (usingcluster && !is.numeric(setBardCluster)) {
	genbasemap<-get(".bardBasemapTMP",envir=.GlobalEnv)
  } else {
	genbasemap <- basemap
  }
  if (tracelevel>0) {
    print(paste("Generating plan #",x)) 
    flush.console()
  }
  retvalgen<-do.call(gen.fun,gen.args)
  return(retvalgen)
 }
 
 genplans<-lapplyBardCluster(seq(1,length=ngenplans),mygenplan )

  retval<-list()
  combinedplans<-cs(seedplans,genplans)
  for (i in seq(1,length=(numevals-1))) {
    combinedplans<-cs(combinedplans,seedplans,genplans)
  }
  
  makeScore<-function(w) {
    retvalscore<-NULL
    dynamicscore<-TRUE
    funformals<-names(formals(score.fun))
    if (!("..." %in% funformals) 
      && !(all(c("lastscore","changelist") %in% funformals) ))       {
      FALSE
    }
    funformals<-names(formals(addscore.fun))
    if (!("..." %in% funformals) 
      && !(all(c("lastscore","changelist") %in% funformals) ))       {
      FALSE
    }
    if (dynamicscore) {
      retvalscore<-function(p,lastscore=NULL,...){
      s1last<-NULL
      s2last<-NULL
      if (!is.null(lastscore)) {
        s1last<-attr(lastscore,"s1last")
        s2last<-attr(lastscore,"s2last")
      }

        s1<-score.fun(p,lastscore=s1last,...)
        s2<-addscore.fun(p,lastscore=s2last,...)
        combined<-weight.fun(s1,s2,w)
        attr(combined,"s1last")<-s1
        attr(combined,"s2last")<-s2
        return(combined)
      }
     return(retvalscore)
    } else {
      retvalscorealt<-function(p){weight.fun(score.fun(p),addscore.fun(p),w)}
      return(retvalscorealt)
    }
 
    # SHOULDN'T REACH HERE
    warning("bug in code")
  }
  for (w in weight) {
    if (tracelevel>0) {
      print(paste("Sampling for weight = ",w))
    }
    thisRound<-samplePlans(
      combinedplans,
      score.fun=makeScore(w),
      ngenplans=0,
      refine.fun=refine.fun, refine.args=refine.args
      )
    retval<-cs(retval,thisRound)
  }
 
 score1<-matrix(sapply(retval,function(x)sum(score.fun(x))))
 score2<-matrix(sapply(retval,function(x)sum(addscore.fun(x))))
 scores<-cbind(score1,score2)
 colnames(scores)<-c(as.character(match.call()$score.fun)[1],as.character(match.call()$addscore.fun)[1]) 
 rownames(scores)<-paste("p",(1:dim(scores)[1]),sep="")
 
 
 attr(retval,"call")<-match.call()
 attr(retval,"scores")<-scores
 class(retval)<-"bardSample" 
 return(retval)
}


#################################
#
# samplePlans
#
# This generates a pseudo-sample of plans  using the same
# score function components.
#
# The function will automatically use a computing cluster for
# the evaluation if one has been started via startBardCluster()
#
#   seedplans - initial plans to be used as seeds
#   score.fun - base score functiion
#   ngenplans - number of additional plans to generate
#   gen.fun - function for generating additional plans
#   gen.args - a list of additional arguments to gen.fun
#   refine.fun - function for plan refinement
#   planRefineArgs - a list of additional arguments to refine.fun
#   numEvals - number of evaluations per plan at each point
#
##################################


samplePlans<-function( seedplans,
   score.fun=calcContiguityScore,ngenplans=24,
   gen.fun = "createRandomPlan", gen.args=list(),
   refine.fun = "refineAnnealPlan", refine.args=list(),
   tracelevel = 1,
   usecluster=TRUE
) {

 if (class(seedplans)!="list") {
  seedplans<-list(seedplans)
 }
 
  if (!checkPlans(seedplans)) {
    return(NULL)
   }
 
 basemap<-basem(seedplans[[1]])
 ndists<-attributes(seedplans[[1]])$ndists
 
 gen.args$ndists<-ndists
 gen.args$basemap<-as.name("genbasemap")

   if ("usecluster" %in% names(formals(eval(as.name(refine.fun))))) { 
   	refine.args$usecluster<-FALSE
   }


  usingcluster<-FALSE
  cl<-setBardCluster()
  if (!is.null(cl) && usecluster) {
  	usingcluster<-TRUE
	if (tracelevel>0) {
		print("Will use cluster for sampling. Note that graphics, and most tracing unavailable")
		flush.console()
	}
	refine.args$displaycount<-NULL
	refine.args$tracelevel<-0
  }
  
 # tricky bit for clusters -- avoid serializing
 # multiple copies of basemaps, either as 
 # args or in plans
 
 mygenplan<-function(x) {
  if (usingcluster && !is.numeric(setBardCluster())) {
	genbasemap<-get(".bardBasemapTMP",envir=.GlobalEnv)
  } else {
	genbasemap <- basemap
  }
  if (tracelevel>0) {
    print(paste("Generating plan #",x)) 
    flush.console()
  }
  retval<-do.call(gen.fun,gen.args)
  return(retval)
 }
 
if (usingcluster && !is.numeric(setBardCluster())) {
	assign(".bardBasemapTMP",basemap,envir=.GlobalEnv)
        on.exit(rm(list=".bardBasemapTMP",envir=.GlobalEnv))
	bardClusterExport(".bardBasemapTMP")
 }

 genplans<-lapplyBardCluster(seq(length.out=ngenplans), mygenplan)
    
 myrefplan<-function(iplan){ 
   if (usingcluster && !is.numeric(setBardCluster())) {
	basem(iplan)<-get(".bardBasemapTMP",envir=.GlobalEnv)
      }
      if (tracelevel>0) {
          print("Refining plan...")
          flush.console()
      }
      myargs<-refine.args
      myargs$plan<-as.name("iplan")
      myargs$score.fun<-as.name("score.fun")
      retval<-do.call(refine.fun,myargs)
    if (usingcluster && !is.numeric(setBardCluster())) {
         basem(retval)<-NULL
      }      
      return(retval)
    }
 
 combinedplans<-cs(seedplans,genplans)
 if (usingcluster && !is.numeric(setBardCluster())) {
	for (i in seq(1,length=length(combinedplans))) {
		basem(combinedplans[[i]])<-NULL 
        } 
 }
	
	if (tracelevel>0) {
		print("Starting refinement phase")
        }
 refinedplans<-lapplyBardCluster(combinedplans,myrefplan)
if (usingcluster && !is.numeric(setBardCluster())) {
	for (i in seq(1,length=length(refinedplans))) {
		basem(refinedplans[[i]])<-basemap
        } 
 }
	if (tracelevel>0) {
		print("Calculating scores")
        }
	
 scores<-matrix(sapply(refinedplans,function(x)sum(score.fun(x))))
 colnames(scores)<-as.character(match.call()$score.fun)[1]
 rownames(scores)<-paste("p",(1:length(scores)),sep="")
 
 
 attr(refinedplans,"call")<-match.call()
 attr(refinedplans,"scores")<-scores
 class(refinedplans)<-"bardSample"
 return(refinedplans)   
}


refineGRASPPlan<-function(plan, score.fun, samplesize=50, predvar=NULL, displaycount=NULL,  
        historysize=0, dynamicscoring=FALSE, usecluster = TRUE, tracelevel=1 )      {
      if (historysize!=0) {
          warning("historysize not useful for  GRASP")
          historysize<-0
      }
      
      # note altering usecluster and tracelevel
      # since already clustered at sample level
      if (tracelevel>0) {
        print("Starting GRASP...")
        flush.console()
      }

     refine.args<-list( displaycount=displaycount, historysize=historysize,
             dynamicscoring=dynamicscoring, 
             tracelevel=tracelevel-1 )


      plans <- samplePlans(seedplans=list(plan), ngenplans=samplesize, score.fun=score.fun,
          gen.fun="createRandomPopPlan", gen.args=list(predvar=predvar),
          refine.fun="refineGreedyPlan", 
          refine.args=refine.args,
          tracelevel=tracelevel,
	  usecluster=usecluster)
      
      if (tracelevel>0) {
        print("Finding best solution...")
        flush.console()
      }       
      scores<-sapply(plans,function(x)sum(score.fun(x)))
      best<-which.max(scores)
      retval<-plans[[best]]
      return(retval)
}                 
      

summary.bardSample<-function(object,...) {
  retval<-attr(object,"scores")
  class(retval)<-"bardSample.summary"
  return(retval)
}

print.bardSample.summary<-function(x,...) {
	inner.print.bardSample.summary(x,...)
}

print.bardSample<-function(x,...) {
	inner.print.bardSample(x,...) 
}
HTML.bardSample.summary<-function(x,...) {
	inner.print.bardSample.summary(x,...,useHTML=TRUE)
}

HTML.bardSample<-function(x,...) {
	inner.print.bardSample(x,...,useHTML=TRUE) 
}

inner.print.bardSample.summary<-function(x,...,useHTML=FALSE) {
	if (useHTML) {
		print<-function(x)HTML(x,...)
		cat<-print
		plot<-hplot
	}
  cat("Distribution of Scores\n\n")
  print(summary(x),...)
  cat("\n\nNumber of samples",dim(x)[1],"\n")
}

inner.print.bardSample<-function(x,...,useHTML=FALSE) {
	if (useHTML) {
		print<-function(x)HTML(x,...)
		cat<function(...)
		plot<-hplot
	}
  print(attr(x,"scores"),...)
}

plot.bardSample<-function(x,ordered=TRUE,...) {
    class(x)<-"list"
    scores<-attr(x,"scores")
    if (ordered) {
   	 plotGrid(x[order(scores)])
    } else {
   	 plotGrid(x)
    }
}

plot.bardSample.summary<-function(x,single=F,...) {
    op <- par(no.readonly=TRUE)
    on.exit(par(op))
      plotSingle<-function(colnum) {   
         par(mfcol=c(2,1))
         plot(unclass(x[,colnum]),ylab=colnames(x)[colnum],xlab="Index",main="Sample Scores",...)
         plot(density(unclass(x[,colnum])),xlab=colnames(x)[colnum],ylab="",main="Density",...)
      }
 
    if (dim(x)[2]==1) {
         plotSingle(1)
    } else {
      par(bg="white")
      pairs(unclass(x))
      if (interactive() && dev.interactive()) {
      	par(ask=TRUE)
      }
      if (!single) {
         sapply(seq(1,length=dim(x)[2]), plotSingle)
      }
    }
    invisible()
}

####################
# Test functions - Used for development testing, not used in running production code.
####################

refineTestPlan <- function( plan, score.fun, displaycount=NULL,  historysize=0, dynamicscoring=FALSE ,    tracelevel=1 ) {
    
    # returns original plan, for testing
    if (tracelevel>0) {
      print("IN refine stub")
      flush.console()
    }
    return(plan)
}
