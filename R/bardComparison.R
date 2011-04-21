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
    if (class(plans)=="bardPlan") {
      plans <- list(plans)
    }
    if (!is.list(scoreFUNs)) {
    	    scoreFUNs=list(scoreFUNs)
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
  	if (is.null(levels(plans[[1]]))) {
  		distnames<-c("Total",1:ndists)
  		distids<-distnames
  	} else {
  		distnames<-c("Total",paste(1:ndists," (",levels(plans[[1]]),")",sep=""))
  		  distids<-c("Total",1:ndists)

  	}
  	retval[["DistrictID"]]<-factor(retval[["DistrictID"]],levels=0:ndists,labels=distnames)
  	retval[["OriginalID"]]<-factor(retval[["OriginalID"]],levels=0:ndists,labels=distids)
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
  plotOpts=NULL,
  useHTML=FALSE,
  completeHTML=FALSE,
  ...
  ) {
   
  if (useHTML) {
	htmlArgs<-list(...)
	print<-function(...)hprint(...,htmlArgs=htmlArgs)
	cat<-function(...)hcat(...,htmlArgs=htmlArgs)
	plot<-function(...)hplot(...,htmlArgs=htmlArgs)
  } else {
	htmlArgs<-NULL
	completeHTML<-FALSE

  }
 
  
  if (completeHTML) {
  	targetDIR<- file.path(tempdir(),"R2HTML")
	dir.create(targetDIR)
	copyR2HTMLfiles(targetDIR)
	target <- HTMLInitFile(targetDIR,filename="sample", BackGroundColor="#BBBBEE", Title="BARD Web Output")
	HTML.title("Redistricting Analysis", HR=2,file=target)
	HTML("<p>Summary of Redistricting Plans</p>",file=target)  
  }
  

       
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
    if (doplot && interactive() && dev.interactive()) {
       op<-par(ask=TRUE)
       on.exit(par(op))
    }
    
    if (dodetails && doplot) {
      cat("Plan matrix plot\n\n")
      if (inherits(plans,"bardSample")) {
        plot(plans)
      } else {
      	tmp <- plans
      	class(tmp)<-"bardSample"
        plot(tmp,ordered=FALSE)
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
   if(completeHTML) {
   	   HTMLEndFile()
   	   retval<-target
   } else {
   	   retval<-NULL
   }
 invisible(retval)
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
    inner.print.bardPlanDiff(x,...)
}

HTML.bardPlanDiff<-function(x,...) {
    inner.print.bardPlanDiff(x,..., useHTML=TRUE)
}

inner.print.bardPlanDiff<-function(x,...,useHTML=FALSE) {
   if (useHTML) {
	htmlArgs<-list(...)
	print<-function(...)hprint(...,htmlArgs=htmlArgs)
	cat<-function(...)hcat(...,htmlArgs=htmlArgs)
	plot<-function(...)hplot(...,htmlArgs=htmlArgs)
  } else {
	htmlArgs<-NULL

  }
  
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

##
## choropleth
##

choroplotPlan<-function(plan,scores,numlevels=5,
	method=c("quant","equal","absolute"),
	main="choropleth map", absmin=0,absmax=1,ramplow="blue",ramphigh="red",...) {
	
	
	methchoice<-match.arg(method)
	switch(methchoice,
	   absolute = {
		levels <- seq(from=absmin, to=absmax, length.out=numlevels+1)[-1]
	   },
	   equal = {
		levels <- seq(from=min(scores), to=max(scores), length.out=numlevels+1)[-1]
	   },
	   quant=  {
		levels <- quantile(scores,prob=seq(0,1,length.out=numlevels+1)[-1])
	   }
	)
	
	if (min(levels)<0 && max(levels)>0) {
		cr<-colorRampPalette(c(ramplow,"white",ramphigh))
	} else if (min(levels)<0) {
		cr<-colorRampPalette(c(ramplow,"white"))
	} else {
		cr<-colorRampPalette(c("white",ramphigh))
	}

	choro.col<-cr(numlevels)
	
	lindex<-sapply(scores,function(x)min(which(x<=levels)))

	res <- list(lindex=lindex, choro.col=choro.col, levels<-levels,main=main,plan=plan)
	class(res)<-"bardChoroplot"
	return(res)
	
}

print.bardChoroplot<-function(x,...) {
	lindex<-x$lindex
	choro.col<-x$choro.col
	levels<-x$levels
	main<-x$main
	plan<-x$plan
	plot(plan,col=choro.col[lindex],...)
	if (!is.null(names(levels))) {
		legnames<-names(levels)
	} else {
		legnames<-format(levels,digits=6)
	}
	legnames<-paste("<=",legnames)
	legend("bottomright", legend=legnames,fill=choro.col)
	title(main=main)
	invisible()
}

plot.bardChoroplot<-function(x,...) {
	print(x,...)
	invisible()
}

PMPreport<-function(
	bardMap,
	blockAssignmentID="BARDPlanID",
	popVar=NULL,
	popVarExtra=NULL,
	ratioVars=NULL,
	splitVars = NULL,
	blockLabelVar=NULL,
	repCompactness=TRUE,
	repCompactnessExtra=FALSE,
	repSpatial=TRUE,
	repSpatialExtra=FALSE,
	useHTML=TRUE,
	districtPolys=NULL,
	...)  {

	#	
	# setup HTML functions
	#
	
	if (useHTML) {
		htmlArgs<-list(...)
		print<-function(...)hprint(...,htmlArgs=htmlArgs)
		cat<-function(...)hcat(...,htmlArgs=htmlArgs)
		plot<-function(...)hplot(...,htmlArgs=htmlArgs)
	} else {
		htmlArgs<-NULL
	}
	
	
	#
	# Setup Plan
	# 
	
	if (class(bardMap)=="bardPlan") {
		tmpPlan <- bardMap
		bardMap <-basem(bardMap)
	} else {
		ow<-options(warn=-1)
		tmpPlan <-createAssignedPlan(bardMap,blockAssignmentID)
		options(ow)
	}
	ndists<-attr(tmpPlan,"ndists")
	dnames<-levels(tmpPlan)
	if (is.null(dnames)) {
		dnames<-1:ndists	
	}
	
	#
	# helper
	# 
	
	# massage score df for single plan
	adjScoreMatrix<-function(sp,avg=T) {
		sp<-sp[c(-1,-3)]
		tlevels <- levels (sp[[1]])
		if (avg) {
			tlevels[which(tlevels=="Total")]<-"Average"
			levels(sp[[1]])=tlevels
			tmeans<- apply(sp[1:dim(sp)[1]-1,-1,drop=F],2,mean)
			sp[dim(sp)[1],-1]<-tmeans
		}
		return(sp)
	}

	
	#
	# Main Scoring Report
	#
	
	if(!is.null(districtPolys)) {
		
		#printTitle("District Map",useHTML=useHTML,htmlArgs=htmlArgs)
		#plot(districtPolys)
	}
	
	
	# popVar - Equal Population
	if (!is.null(popVar)) {
		printTitle("Total Population",useHTML=useHTML,htmlArgs=htmlArgs)
		
		scoreFUN=list("Population"=function(x)calcPopScore(x,predvar=popVar[[1]],standardize=FALSE))
		names(scoreFUN)[1]<-names(popVar)[1]
		
		scores <- scorePlans(tmpPlan, scoreFUN=scoreFUN)
		scores<-adjScoreMatrix(scores)
		scores<-scores[1:dim(scores)[1]-1,]
		
		ptarget <-sum(bardMap$df[popVar[[1]]])/ndists
		target<-abs((scores[,2]-ptarget)/ptarget)<popVar[[2]]
		scores[3]<-target
		names(scores)[3]<-"Within target range"
		print(scores)
	}
	
	
	# popVarExtra - demographics
	if (!is.null(popVarExtra)) {
		for (i in 1:length(popVarExtra)) {
			printTitle(names(popVarExtra)[i],useHTML=useHTML,htmlArgs=htmlArgs)
			scoreFUN=list("Population"=function(x)calcPopScore(x,predvar=popVarExtra[[i]],standardize=FALSE))
			names(scoreFUN)[1]<-names(popVarExtra)[i]
		
			scores <- scorePlans(tmpPlan, scoreFUN=scoreFUN)
			scores<-adjScoreMatrix(scores)
			gini<-calcIneqScore(tmpPlan,popVarExtra[[i]])
			gini<-c(gini,mean(gini))
			scores[3]<-gini
			names(scores)[3]<-"District Homogeneity"
			print(scores)
		}
	}
	
	# ratioVars
	if (!is.null(ratioVars)) {
	   for (i in 1:length(ratioVars)) {
	   	varset=ratioVars[[i]]
		denominator<-varset$denominator
		printTitle(names(ratioVars)[i],useHTML=useHTML,htmlArgs=htmlArgs)
		if(is.null(denominator)) {
			denominator<-popVar[1]
		}
		threshold<-varset$threshold
		for (j in 1:length(varset$numerators))  {
			
			ratscore<-calcRangeScore(tmpPlan,predvar=varset$numerators[[j]],
				predvar2=denominator[[1]],standardize=F,sumdenom=F)
			ratscoredf<-cbind(as.data.frame(dnames),
				attr(ratscore,"rawscore1"),
				ratscore,
				ratscore>=threshold
				)
			names(ratscoredf)<-c("District",names(varset$numerators)[j],
			paste("Proportion of ",names(denominator),sep=""), 
			paste(">= ",threshold,sep="") )
		
			print(ratscoredf)
		}
	   }
	}
	
	# splitVars

	if(!is.null(splitVars)) {					
		printTitle("District Splits",useHTML=useHTML,htmlArgs=htmlArgs)
		
		splitscore<-sapply(splitVars,function(x)calcSplitScore(tmpPlan,splitvar=x,standardize=F))
		colnames(splitscore)<-names(splitVars)
		splitdf<-cbind("District"=dnames,as.data.frame(splitscore))
		print(splitdf)
		
	}
		
	
	
	# compactness
	# compactness extras
	
	if (repCompactness) {
		if (!repCompactnessExtra) {
			sFUN<-list("Length-Width Compactness"=calcLWCompactScore)
		} else { 
			sFUN<-list("Length-Width Compactness"=calcLWCompactScore,
			"Bounding Circle Compactness"=function(x)calcReockScore
				(x,usebb=TRUE))
		}
		printTitle("Compactness scores",useHTML=useHTML,htmlArgs=htmlArgs)
		scores<- scorePlans(tmpPlan,scoreFUN=sFUN)
		scores<- adjScoreMatrix(scores)
		print(scores)
	}
	
	
	# spatial
	# spatial extras
	if (repSpatial) {
		printTitle("Unassigned Geography",useHTML=useHTML,htmlArgs=htmlArgs)
		holes<-which(is.na(tmpPlan))

		if (length(holes)==0) {
			print("No unassigned geography detected -- plan is complete.")
		} else {
			print("Plan is incomplete.")
			if (!is.null(blockLabelVar)){
			badBlocks<-
			as.data.frame(bardMap)[[blockLabelVar]][holes]
			} else {
				badBlocks<-holes
			}
			print(paste("number of unassigned blocks",length(badBlocks)))

			if (length(badBlocks<100)) {
				print("Unassigned blocks...")
				print(paste(badBlocks," "))

			}
		}
	}
	
	if (repSpatialExtra) {
		printTitle("Plan Contiguity",useHTML=useHTML,htmlArgs=htmlArgs)
		
		cscores<-scorePlans(tmpPlan,list("Number of Discontigous Areas" = function(x){calcContiguityScore(tmpPlan,standardize=F)-1}))
		cscores<-adjScoreMatrix(cscores,avg=F)

		if (sum(cscores[,2])==0) {
			print("Plan is contiguous")
		} else {
			print("Plan is not contiguous")
			print(cscores)
			
		}
		
		# need to optimize with supplied aggregated polys
		DOHOLES<-F
		if (DOHOLES) {
		  printTitle("Districts with Donut Holes",useHTML=useHTML,htmlArgs=htmlArgs)
		  hscores<- calcSpatialHolesScore(tmpPlan,standardize=F)
		
		
		  hscores<-scorePlans(tmpPlan,list("Number of holes" = function(x){calcSpatialHolesScore(tmpPlan,standardize=F)}))
		  hscores<-adjScoreMatrix(hscores,avg=F)

		
		  if (sum(hscores[,2])==0) {
			print("No holes found.")
		  } else {
			print("Some donut holes (areas owned by other districs)...")
			print(hscores)
			
		  }
		}

	}

}
