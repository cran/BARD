##############################################################################
#
# bard plan comparison
#
# 
# This module contains methods to compare a set of  plans.  
#
##############################################################################

#########################################
#
# diff.bardPlan
#
# returns set of plan differences
#
# arguments
#     plan1, plan2 - plans
#     doMatch - rematch district ID's
#
# SEE R HELP FILE  FOR FULL DETAILS
#
#########################################

diff.bardPlan<-function(x,plan2,domatch=TRUE,...) {
  plan1<-x
  # input checks
  if (!checkPlans(list(plan1,plan2))) {
    return(NULL)
  }
  districtonly<-is.districtonly(x)
  if (districtonly) {
  	domatch<-FALSE
  }
  
  #recoding to match plan IDorder
  if (districtonly) {
  	planIDs<-1
  	} else {
  	planIDs<-sort(unique(c(unique(plan1),unique(plan2))))
  }
  if (domatch) {
    matchID<-matchPlanID(plan1,plan2)
  } else {
    matchID<-NULL
  }
  if (!is.null(matchID)) {
    plan2recode<- recodePlan(plan2,matchID)
  } else {
    plan2recode<-plan2
  }
  
  # construct difference list
  retval<- sapply(planIDs, simplify=FALSE, 
    function(id) {
      p1 <- which(plan1==id)
      p2 <- which(plan2recode==id)
      list(shared=intersect(p1,p2),p1Only=setdiff(p1,p2),p2Only=setdiff(p2,p1))
    }
  )

  p1<-which(is.na(plan1))
  p2<-which(is.na(plan2))
  attr(retval,"holes")<-
  list(shared=intersect(p1,p2),p1Only=setdiff(p1,p2),p2Only=setdiff(p2,p1))
  attr(retval,"plan1")<-plan1
  attr(retval,"plan2")<-plan2
  attr(retval,"plan2recode")<-plan2recode
  attr(retval,"matchID")<-matchID 
  attr(retval,"districtonly")<-districtonly
  class(retval)<-"bardPlanDiff"
  return(retval)
}

#################################
#
# scorePlans
#
# creates a data frame of scores
#
# Arguments
#   plans- list of plans, with optional names
#   scoreFUNs - list of score functions
#
##################################

scorePlans<-function(plans, scoreFUNs, domatch=TRUE) {
  # wrap raw plan object
    if (!is.list(plans)) {
      plans <- list(plans)
    }
   if (!checkPlans(plans)) {
    return(NULL)
  }
    districtonly<-is.districtonly(plans[[1]])
    if (districtonly) {
    	domatch<-FALSE
    }
  # simplifies applying one score
    scorePlan <- function(scoreFUN, plan) {
        retval<-tmpScores <- scoreFUN(plan)
        
        if (!districtonly) {
        	if (length(tmpScores) == 1) {
               retval <- c(replicate(ndists, NA), tmpScores)
            } else {
            	retval <- c(tmpScores, sum(tmpScores))
        	}
        }
        return(retval)
    }
  
  # build score data frame
  ndists <- attr(plans[[1]],"ndists")
  retval<-NULL
  matchID<-NULL
  for (i in seq(start=1,length=length(plans))) {
    if (i>1 && domatch) { 
          matchID<-matchPlanID(plans[[1]],plans[[i]])
          tmpplan <- recodePlan(plans[[i]],matchID)
    } else {    
          matchID<-1:ndists 
          tmpplan <- plans[[i]]
    }
    tmpscore<-sapply(scoreFUNs,scorePlan,tmpplan) 
    if (districtonly) {
    	tmpscore<-c(i,tmpscore)
    } else {
        tmpscore<-cbind(rep(i,ndists+1),c((1:ndists),0),c(match(c(1:ndists),matchID),0),tmpscore)
    }
    retval<-rbind(retval,tmpscore)
  }
  
  # names processing
  retval<-as.data.frame(retval)
  scorenames<-makeNames(scoreFUNs,"score")
  plannames<-makeNames(plans)
  rownames(retval)<-NULL


  if (districtonly) {
  	 names(retval)<-c("Plan",scorenames)
  } else {
  	names(retval)<-c("Plan","DistrictID","OriginalID",scorenames)
  	distnames<-c("Total",1:ndists)
  	retval[["DistrictID"]]<-factor(retval[["DistrictID"]],levels=0:ndists,labels=distnames)
  	retval[["OriginalID"]]<-factor(retval[["OriginalID"]],levels=0:ndists,labels=distnames)
  }
  retval[["Plan"]]<-factor(retval[["Plan"]],labels=plannames)

  return(retval)
}

#################################
#
# reportPlans
#
# This is a convenience wrapper for producing a report with
# print, summary, plot, diff, and scorePlans
#
# Arguments
#   plans - a list of plan assignments. 
# 
#   verbose - FLAG. Print detailed information
#
# Returns
#   Nothing. Used for printing and plotting side effects.
#
# LIMITATIONS
#   All plans being compared must have same number of distrists 
#   All plans must have same numer of basemaps
#
##################################

reportPlans<-function(
  plans,
  scoreFUNs=list(
    "Contiguity"=calcContiguityScore,
    "Holes"=calcHolesScore,
    "LW Compact"=calcLWCompactScore,
    "Reock"=calcReockScore
  ), 
  doplot=FALSE, 
  domatch=TRUE,
  dodiff=TRUE,
  dodetails=FALSE,
  doprofileextras=TRUE,
  plotOpts=NULL
  ) {
   

       
  # input processing  & setup
    if (!is.list(plans)) {
      plans <- list(plans)
    }

   if (!checkPlans(plans)) {
    return(NULL)
   }
  
   if (doprofileextras && inherits(plans,"bardSample")) {
      cat("Profile summary\n\n") 
      print(summary(plans))
      if (doplot) {
        plot(summary(plans))
      }
   }
      

    names(plans)<-makeNames(plans)
    nplans<-length(plans)
    if (doplot && interactive()) {
       op<-par(ask=TRUE)
       on.exit(par(op))
    }
    
    if (dodetails && doplot) {
      cat("Plan matrix plot\n\n")
      if (inherits(plans,"bardSample")) {
        plot(plans)
      } else {
        plotGrid(plans)
      }
    }
    
  # print score comparisons
  
  cat("Plan Scores\n\n")
  distscores <- scorePlans(plans,scoreFUNs,domatch=domatch)
  print(distscores)

  # print differences
  if (dodiff && nplans>1) {
   cat("\n\nPlan Differences\n\n")
   for (i in seq(2,length=(nplans-1))) {
     cat("\nComparing plan",names(plans)[1],"with plan",names(plans)[i],":\n\n")
     pdiff<-diff(plans[[1]],plans[[i]],domatch=domatch)
     print(summary(pdiff))
     if (dodetails){
      print(pdiff)
     }
     if (doplot) {
      #plot(pdiff,plotall=TRUE)
	do.call("plot",c(list(pdiff),plotOpts))
     }
    }
   }

 invisible()
}


##############################################################################
#           INTERNAL MODULE FUNCTIONS -- DO NOT EXPORT
##############################################################################


printPlanScoresByDistrict<-function(plan1,plan2, 
  scores, scoreTotal,
  verbose = FALSE ) {
  
  
  distScores1 <- sapply(scores,function(f)f(plan1))
  colnames(distScores1)<-paste("Plan1:",  colnames(distScores1))
  distScores2 <- sapply(scores,function(f)f(plan2))
  colnames(distScores2)<-paste("Plan2:",  colnames(distScores2))
  distScores<-cbind(distScores1,distScores2)

  totals <- apply(distScores,2,scoreTotal)
  distScores<-rbind(distScores,totals)
  rownames(distScores)=c(paste("District",1:(dim(distScores)[1]-1)),"Totals")
  print(distScores)    
  invisible()
}





##########################################
#
# matchPlans
#
# Find reordeing of plan2 id's that best matches plan1.
# Purely heuristic.
#
# Arguments
#   plan1,plan2 - plan assignment vectores
#
# Returns
#   id vector, reflecting permutation of plan1 id's
#
# WARNING -- Assumes plan1 and plan2 both have same # of districts.
#
######################################

matchPlanID<-function(plan1,plan2){
  planID1<-sort(unique(plan1))
  planID2<-sort(unique(plan2))
  
  id.grid<-expand.grid(planID1,planID2)
  id.scores<- cbind(id.grid, 
    mapply(function(x1,x2)sum(intersect(which(plan1==x1),which(plan2==x2))),
        id.grid[[1]],id.grid[[2]])
    )
   names(id.scores)<-c("id1","id2","intersect")
   
   newIDs<-planID2
   for ( id in planID2 ) {
      tmpBest <- which.max(id.scores[[3]])[1]
      tmpID2 <- id.scores[tmpBest,2]
      tmpID1 <- id.scores[tmpBest,1]
      newIDs[tmpID2] <- tmpID1
      id.scores <- id.scores[which(id.scores[[1]]!= tmpID1),]
      id.scores <- id.scores[which(id.scores[[2]]!= tmpID2),]
   }  
   return(newIDs)  
}

##########################################
#
# recodePlans
#
# Recode plan assignment to use new id's
#
# Arguments
#   plan - plan assignment vector
#   matchIDs - id vector as returned from matchPlans
#
# Returns
#   recoded plan vector
#
# WARNING -- Assumes plan1 and plan2 both have same # of districts.
#
######################################

recodePlan<-function(plan,matchID) {
  recodeList = vector(length=length(match),mode="list")
  for (i in 1:length(matchID)) {
     recodeList[[i]]=which(plan==i)
  }
  for (i in 1:length(matchID)) {
     plan[recodeList[[i]]]=matchID[i]
  }
  return(plan)
}


###################
# 
# S3 Generics
#
####################


summary.bardPlanDiff<-function(object,...) {
    x<-object
    pdiffs = x
    plan1 <- attr(x,"plan1")
    matchID <- attr(x,"matchID")
    districtonly<-attr(x,"districtonly")


    if (districtonly) {
    	matchID<-1
    } else if (is.null(matchID)) {
        matchID<-1:attr(plan1,"ndists")
    }
    
    shared<- function(x){
        retval<-c(length(x$p1Only)+length(x$shared),length(x$p1Only),length(x$p2Only),
          signif(digits=3,100*length(x$shared)/
          (length(x$shared)+length(x$p1Only)+length(x$p2Only))))
        is.na(retval)<-!is.finite(retval)
        return(retval)
          }
    
    distTable <- t(sapply(pdiffs,shared))
    
    distTable <- as.data.frame(cbind(1:length(matchID),match(1:length(matchID),matchID),distTable))
    distTable <- rbind( distTable, c(NA,NA,shared(attr(object,"holes"))) )
    
    names(distTable) = c("Dist ID","Original ID","# of original blocks","# Blocks Removed",
      "#  Added", "% Shared")
    rownames(distTable)[dim(distTable)[1]]<-"Holes"
    class(distTable)<- c("bardPlanDiff.summary","data.frame")
    return(distTable)
}

print.bardPlanDiff<-function(x,...) {
    pdiffs = x
    plan1 <- attr(x,"plan1")
    matchID <- attr(x,"matchID")
    if (is.null(matchID)) {
        matchID<-1:attr(plan1,"ndists")
    }
    
    printDiff<-function(pdi) {
      
        cat("Blocks removed: \n")
        if (length(pdi$p1Only)==0) {
          print("[none]")
        } else {
          print(pdi$p1Only)
        }
        cat("\n")
        cat("Blocks added:\n")
        if (length(pdi$p2Only)==0) {
          print("[none]")
        } else {
          print(pdi$p2Only)
        }
        cat("\n")
        cat("**************************************************\n\n")
    }

      for ( i in 1:length(pdiffs)) {
           cat(paste("Plan 1 District ID: ", i, "\n\n"))
           if (!is.null(matchID)) {
           	cat(paste("Plan 2 Matched Original ID: ", match(i,matchID), "\n\n")) 
           	
           }

           printDiff(pdiffs[[i]])
      }
    cat(paste("Holes \n\n"))
    printDiff(attr(x,"holes")) 
    invisible()
}

plot.bardPlanDiff<-function(x,plotall=F, horizontal=T,col=NULL, ...) {
  plan1<-attr(x,"plan1")
  plan2<-attr(x,"plan2recode")
  districtonly<-attr(x,"districtonly")
  op<-NULL
  
  if (districtonly) {
  	colors<-heat.colors(3)
  } else {
  	colors<-col
  }
  
  if (plotall) {
    tp1<-plan1
    tp2<-plan2
    if (horizontal) {
	    op<-par(mfcol=c(1,3), mfcol=c(3,1),mar=c(.1,.1,.1,.1),mai=c(.1,.1,.1,.1))
	    } else {
	   op<-par(mfcol=c(3,1), mfcol=c(3,1),mar=c(.1,.1,.1,.1),mai=c(.1,.1,.1,.1))
	 }

     if (districtonly) {
	 	is.na(tp1)<-(plan1!=1)
	 	tp1[plan1==1]<-2
	 	is.na(tp2)<-(plan2!=1)
	 	tp2[plan2==1]<-3	
    }
    plot(tp1,main="plan 1",col=colors,...)
    plot(tp2,main="plan 2",col=colors,...)
	 	
  
  }
  dplan<-plan1
  if (districtonly) {
   	is.na(dplan)<-TRUE
   	dplan[plan2==1]<-3
   	dplan[plan1==1]<-2
   	dplan[(plan1==1) & (plan2==1)]<-1
   	
   	plot(dplan,main="District Overlap",col=colors,...)

  } else {
  	is.na(dplan)<-(plan1!=plan2)
  	plot(dplan,main="Plan Overlap",col=colors,...)

  }
  	 
  par(op)
  invisible()
}

