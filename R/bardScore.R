##############################################################################
#
# bard scoring functions
#
# 
# This module contains methods to generate scores and update them
# with incremental changes in the district.
#
# The public functions generate scores for districting _plans_.
#
# Most of these rely on internal functions for calculating scores on each
# district. 
#
################################################################################

############################################################################## 
#
#  Plan Scores
# 
# calc*Score -  a plan score, typically built from district scores using
#         calc*ScoreD - district scores, provides a score for a selected district
#
#
# Plan score *all* accept at least the following arguments:
#       - plan - the plan
#       - standardize (default, TRUE), standardizes resulting scores
#  
# A plan score that supports dynamic updates should accept the following additional
# argument:
#       - lastscore - score vector returned from the previous call to the calc*Score
#         function
#       - changelist - a matrix of 
#            -- (column1) block ID's that were changed since lastscore was computed 
#            -- (column2) previous plan assignments for those blocks
#
# Other arguments are optional
#
# A plan score function returns:
#      Either a vector of scores, one for each district,
#      or a single scalar value (if the score is calculated only on a plan
#      basis, which is rare).
#
#     If the "standardize" option is true. Each of these values should be       
#      standardized to  [0,1], with 0 
#      representing  the "best" score and 1 the worst score
#
#
#    When standardized is false, score values MAY return values in other scalar
#    ranges, and even invert the ranking.
#
#    The returned scores MAY have attributes useful for incremental updating
#     when passed back as the argument of lastscore
#
#############################################################################

#########################################################################
#
# calcHolesScore
# 
# Returns a score based on the number of holes in the plan.
# The ideal district has all blocks assigned
#
# Arguments:
#     As above.
#
# Returns:
#     Standardized, as above.
#
#    SEE R HELP FILES FOR COMPLETE DOCS
# 
#########################################################################


calcHolesScore<-function(plan,
  lastscore=NULL, changelist=NULL, standardize=TRUE ) {
  
  if(is.districtonly(plan)) {
  	return(NA)
  }

  nholes<-sum(is.na(plan))
  if(standardize) {
    score <- nholes/length(plan)
  } else {
    score <- nholes
  }
  return(score)
}

calcUnassignedScore<-calcHolesScore

#########################################################################
#
# calcContiguityScore
# 
# Returns a score based on the number of separate contiguous regions in the district.
# The ideal district comprises a single contiguous region.
#
# Arguments:
#     As above.
#
# Returns:
#     Standardized, as above.
#
#    SEE R HELP FILES FOR COMPLETE DOCS
# 
#########################################################################

calcContiguityScore<-function(plan,
  lastscore=NULL, changelist=NULL, standardize=TRUE ) {

   setup <- dynamicScoreSetup(plan, lastscore, changelist)
    score <- setup$score
    if (!is.null(changelist)) {
	    distids<-sameNeighbors(plan,lastscore,changelist)
    } else {
    	distids<-setup$distids
    }    	
    score[distids] <- unlist(sapply(distids, function(x) calcContiguityScoreD(plan, 
        x, standardize)))
    return(score)
}


#########################################################################
#
# calcLWCompactScore
# 
# Returns a compactness score based on the ratio of the length of the bounding
# rectangle for the district
#
# Arguments:
#     As above.
#
# Returns:
#     Standardized, as above.
# 
#    SEE R HELP FILES FOR COMPLETE DOCS
# 
#########################################################################

calcLWCompactScore<-function(plan,
  lastscore=NULL, changelist=NULL, standardize=TRUE ) {

  setup<-dynamicScoreSetup(plan,lastscore,changelist)
  distids<-setup$distids
  score<-setup$score
  
    score[distids]<-sapply(distids,function(x)
      calcLWCompactScoreD(plan,x,standardize))
    return(score)
}

calcReockScore<-function(plan, usebb=FALSE,
  lastscore=NULL, changelist=NULL, standardize=TRUE ) {

  
    setup<-dynamicScoreSetup(plan,lastscore,changelist)
  distids<-setup$distids
  score<-setup$score
  
    score[distids]<-sapply(distids,function(x)
      calcReockScoreD(plan,x,usebb=usebb,standardize=standardize))
    return(score)
}

calcPACompactScore<-function(plan,
  lastscore=NULL, changelist=NULL, standardize=TRUE ) {

  setup<-dynamicScoreSetup(plan,lastscore,changelist)
  distids<-setup$distids
  score<-setup$score  
  
    score[distids]<-sapply(distids,function(x)
      calcPACompactScoreD(plan,x,standardize))
    return(score)
}

calcSpatialHolesScore<-function(plan,
  lastscore=NULL, changelist=NULL, standardize=TRUE ) {

  setup<-dynamicScoreSetup(plan,lastscore,changelist)
  distids<-setup$distids
  score<-setup$score  
  
    score[distids]<-sapply(distids,function(x)
      calcSpatialHolesScoreD(plan,x,standardize))
    return(score)
}

calcBBCompactScore<-function(plan,
  lastscore=NULL, changelist=NULL, standardize=TRUE ) {

  setup<-BARD:::dynamicScoreSetup(plan,lastscore,changelist)
  distids<-setup$distids
  score<-setup$score
  basemap<-basem(plan)

  
  for (i in distids) {
    blocks<-which(plan==i)
    if (length(blocks)==0) {
      score[i]<-1
    } else {
      bb<-BARD:::getBbox(basemap,blocks)
      bb <- (bb$boxMax[1]-bb$boxMin[1])*(bb$boxMax[2]-bb$boxMin[2])
      tarea<-sum(basemap$areas[blocks])
      score[i]<- 1-(tarea/bb)
    }
  }
    return(score)
}

calcSplitScore<-function(plan,splitvar,
  lastscore=NULL, changelist=NULL, standardize=TRUE ) {
  
  ndists<-attr(plan,"ndists")
  if (length(splitvar)==1) {
    	    splitvar<-basem(plan)$df[[splitvar]]
  }   
  
  xt<- xtabs(~splitvar+plan)
  splitnames<-names(xt[,1])
  splitl <- (apply((xt>0),1,sum)>1)
  
  if (standardize) {
  	  # percentage of ID's split
  	  score <- sum(splitl)/length(splitnames)
  } else {
  	  score <- numeric(ndists)
  	  dnames<-names(xt[1,])
  	  # district by district scores
  	  for (i in 1:ndists) {
  	  	ic<-as.character(i)
  	  	if (!ic %in% dnames) {
  	  		score[i]<-0
  	  	} else {
  	  		score[i]<- sum(xt[,ic]>0 & splitl)
  	  	}
  	  }
  	  	
  	 
  }
  return(score)

}

#########################################################################
#
# calcPopScore
# 
# Returns a score based on the population equality of the districts.
#
# Arguments:
#     As above.
#    
#    Additionally:
#     - predvar - name of basemap variable containing block population
#
# Returns:
#     Standardized, as above.
# 
#    SEE R HELP FILES FOR COMPLETE DOCS
# 
#########################################################################

calcPopScore<-function(plan, predvar="POP",
  lastscore=NULL, changelist=NULL, standardize=TRUE ) {

  basemap<-basem(plan)
  ndists<-attr(plan,"ndists")
  districtonly<-is.districtonly(plan)
  if (districtonly) {
  	rawscore<-calcUniScoreD(plan,1,varid=predvar)
  } else   if(is.null(lastscore) || is.null(attr(lastscore,"rawscore"))) { 
    rawscore<-sapply(1:ndists,function(x)calcUniScoreD(plan,x,varid=predvar))
  } else {
    rawscore <- attr(lastscore,"rawscore")
    for ( i in seq(along.with=changelist[,1]) ) {
      blockval <- basemap$df[[predvar]][changelist[i,1]]
      if (!is.na(changelist[i,2])) {
         rawscore[changelist[i,2]] <- rawscore[changelist[i,2]]-blockval
      }
      if (!is.na(changelist[i,1])) {
         rawscore[plan[changelist[i,1]]] <- rawscore[plan[changelist[i,1]]] +  blockval
      }
    }
  }
  
  if (standardize) {
  	
  	if (districtonly) {
  		totalv<-sum(basemap$df[predvar]) 
  		} else {
  		totalv<-sum(rawscore)
  	}
      score<-  sqrt(abs((rawscore-totalv/ndists))/sum(totalv))
  } else {
      score<-rawscore
  }
  attr(score,"rawscore")<-rawscore
  return(score)
}

#########################################################################
#
# calcRangeScore
# 
# Calculates disctricts compliance to a target range for a predictive variable.
# Will penalize districts increasingly as sum(prevar1)/(sum(predvar1)+sumpredvar(2))
# falls outside the given target range.
#
# Use this for majority-minority districts, partisan districts, competitive districts
#
# Arguments:
#     As above.
#
#     Additionally -
#           - predvar1 - name of predictive variable 1   in basemap
#           - predvar2 - ame of predictive variable 2   in basemap
#
# Returns:
#     Standardized, as above.
# 
#    SEE R HELP FILES FOR COMPLETE DOCS
#                  
#########################################################################

calcRangeScore<-function(plan, predvar="BLACK", predvar2="WHITE", 
	targrange=c(.65,.70), sumdenom=TRUE,
  lastscore=NULL, changelist=NULL, standardize=TRUE ) {
  
  if ((length(targrange)!=2) || min(targrange<0) || max(targrange>1) || 
    (targrange[1]>targrange[2])) {
    stop("Illegal range")
  }

  ndists<-attr(plan,"ndists")
  basemap<-basem(plan)
  if (is.districtonly(plan)) {
  	    rawscore1<-calcUniScoreD(plan,1,varid=predvar)
  	    rawscore2<-calcUniScoreD(plan,1,varid=predvar2)
  } else   if(is.null(lastscore) || is.null(attr(lastscore,"rawscore"))) { 
    rawscore1<-sapply(1:ndists,function(x)calcUniScoreD(plan,x,varid=predvar))
    rawscore2<-sapply(1:ndists,function(x)calcUniScoreD(plan,x,varid=predvar2))
  } else {
    rawscore1 <- attr(lastscore,"rawscore1")
    rawscore2 <- attr(lastscore,"rawscore2")
    for ( i in seq(along.with=changelist[,1]) ) {
      blockval1 <- basemap$df[[predvar]][changelist[i,1]]
      blockval2 <- basemap$df[[predvar2]][changelist[i,1]]
      if (!is.na(changelist[i,2])) {
        rawscore1[changelist[i,2]] = rawscore1[changelist[i,2]]-blockval1
        rawscore2[changelist[i,2]] = rawscore2[changelist[i,2]]-blockval2
      }
      if (!is.na(changelist[i,1])) {
        rawscore1[plan[changelist[i,1]]] = rawscore1[plan[changelist[i,1]]] +  blockval1
        rawscore2[plan[changelist[i,1]]] = rawscore2[plan[changelist[i,1]]] +  blockval2
      }
    }
  }
  if (sumdenom) {
        ratios<-(rawscore1/(rawscore1+rawscore2))
        } else {
        ratios<-(rawscore1/rawscore2)
  }
  if (standardize) {
      # penalty function for being outside range 
       
      score<-pmin(abs(ratios - targrange[2]),abs(ratios-targrange[1]))
      score[ratios >= targrange[1] & ratios <=targrange[2]]<-0
      score[which(is.nan(ratios))]<-1
  } else {
      #report range
      score<-ratios
  }
  attr(score,"rawscore1")<-rawscore1
   attr(score,"rawscore2")<-rawscore2
  return(score)
}

#########################################################################
#
# calcIneqScore
# 
# Calculates plans distributional (in)equality using the ineq package
#
# Arguments:
#     As above.
#
#     Additionally -
#           - eqvar - name of variable to be used for inequality measure
#	    - weightVar - weight variable
#	    - type - type of inequality score, per ineq
#	    - parameter - parameter for inequality score
#
# Returns:
#     Standardized, as above.
# 
#    SEE R HELP FILES FOR COMPLETE DOCS
# 
#########################################################################

calcIneqScore<-function(plan,eqvar,weightVar=NULL,parameter=NULL,  type = c("Gini", "RS", "Atkinson", "Theil", "Kolm", "var", "square.var", "entropy"),
           lastscore=NULL, changelist=NULL, standardize=TRUE) { 

  if (length(type)>1) {
  	type<-type[1]
  }
  setup<-dynamicScoreSetup(plan,lastscore,changelist)
  distids<-setup$distids
  score<-setup$score
  
  if(!mrequire("ineq",quietly=TRUE,warn.conflicts=FALSE) && (type!="var")) {
  	warning("Ineq package not installed. Returning coef of variation.")
  	type<-"var"
  }
  
 
  score[distids]<-sapply(distids,function(x)
      calcIneqScoreD(plan,x,eqvar=eqvar,weightVar=weightVar,parameter=parameter,type=type))
      
      if (standardize && (type %in% c("Kolm","Theil","entropy","var","square.var"))) {
      	      	score <- 1-1/(score+1)

      }
   
  return(score)
}



#########################################################################
#
# calcGroupScore
# 
# Calculates plans compliance with keeping designated groups. Use for 
# designated "nesting" districts, or known communities of interest
#
# Arguments:
#     As above.
#
#     Additionally -
#           - groups - a list of vectors, each vector should comprise the blocks
#                      in that group, groups may overlap
#           - penalties - a single number, or vector of penalties associated with
#                      splitting each of the enumerated groups
#
# Returns:
#     Standardized, as above.
#     Number of groups split per district. (If a group is split across multiple
#     district, each district is penalized for the split)
# 
#    SEE R HELP FILES FOR COMPLETE DOCS
# 
#########################################################################

calcGroupScore<-function(plan,groups=list(),penalties=1,
           lastscore=NULL, changelist=NULL, standardize=TRUE) { 

 if (length(groups)==0) {
  warning("no groups specified")
  return(0)
 }


   setup<-dynamicScoreSetup(plan,lastscore,changelist)
  distids<-setup$distids
  score<-setup$score
 
    score[distids]<-sapply(distids,function(x)
      calcGroupScoreD(plan,x,groups,penalties))
    return(score)
}

#########################################################################
#
# calcMomentScore
# 
# Calculates plans compliance with moment of inertia, can be used for general
# penalized distrance scores
#
# Arguments:
#     As above.
#
# Returns:
#     Standardized, as above.
#     Moment of inertia
# 
#    SEE R HELP FILES FOR COMPLETE DOCS
# 
#########################################################################

calcMomentScore<-function(plan,standardize=TRUE,centers=NULL,weightVar=NULL,penaltyExp=2, 	normalize=TRUE,lastscore=NULL, changelist=NULL ) { 

           
  setup<-dynamicScoreSetup(plan,lastscore,changelist)
  distids<-setup$distids
  score<-setup$score
  ndists<-attr(plan,"ndists")

  
  
 if ((!is.null(centers)) &&length(centers)!=ndists) {
  warning("wrong number of centers specified")
  return(NA)
 }
 if  (!is.null(weightVar) && is.null(basem(plan)$df[weightVar])) {
 	  warning("nonexistent weight variable  specified")
	  return(NA)
 }
    score[distids]<-sapply(distids,function(x)
      calcMomentScoreD(plan,x,standardize,centers,weightVar,penaltyExp,normalize=normalize))
    return(score)
}

#########################################################################
#
# combineDynamicScores
# 
# Helper function to combine dynamic scores
#
#    SEE R HELP FILES FOR COMPLETE DOCS
# 
############################################################################

combineDynamicScores<-function(plan,lastscore=NULL,changelist=NULL,scorefuns=list(),
	distcombfun=sum,scorecombfun=sum) {
	if (!is.null(changelist) && !is.null(lastscore) && !is.null(attr(lastscore,"lastscorev"))) {
		lastscorev <-attr(lastscore,"lastscorev")
		scores<-vector(length=length(scorefuns))

		for (i in seq(1,length=length(scorefuns)))  {
		   scores[i]<-
			scorefuns[i](plan,lastscore=lastscorev[i],changelist=changelist)
		}
	} else {
		scores<-lapply(scorefuns,function(x)x(plan))
	}
	
	retval<-scorecombfun(sapply(scores,distcombfun))
	attr(retval,"lastscorev")<-scores
	return(retval)
}



##############################################################################
#           INTERNAL MODULE FUNCTIONS -- DO NOT EXPORT
###############################################################################

#############################################################################
#
# District scoring functions
# 
# all are of the form calc*ScoreD
#
# They all take a _plan_assignment_vector_ ( a bardPlan object, see bardPlan.R)
# an a basemap (a bardBasemap object,  bardUtil.R), and a district id. They return 
# a district level score.
#
# - plan - bardPlan
# - basemap - bard map
# - distid - district id
#
# They may take additional arguments, but not all plan functions are
# guaranteed to accept them.
#
##############################################################################

###########################################################################
#
# calcContiguityScoreD
#
# returns the number of contiguous regions, see calcCotiguityScore
#
##########################################################################

calcContiguityScoreD<-function(plan,distid,standardize=TRUE) {
    blocks<-(plan==distid) 
    blocks<-na.omit(blocks)
    if (sum(blocks)==0) {
     if (standardize) {
      return(1)
     } else {
      return(0)
     }
    }
    basemap<-basem(plan)
    nb<-basemap$nb
    
    # Standard spdep functions need this:
    #
    # subnb <- subset(basemap$nb, blocks)   
    # regions <-  n.comp.dist(subnb,blocks)$nc
    #
    # but its very slow, so we coded our own n.comp.dist
    
    nregions <-  n.comp.include(nb,blocks)$nc
    # adjust for entirely missing districts
    if (nregions==0) {
	nnregions<-1
    }
    score<-nregions
    if (standardize) {
      score <- 1-(1/nregions)
    }
    
    return(score)
}

#########################################################################
#
#  calcLWCompactScoreD
#
# Returns the LW compactness as determined bycalculating the minimum and maximum
# x and y coordinates.  Then a bounding box is constructed and the area is
# calculated/
#
#  See calcLWCompactScore
#
#########################################################################

calcLWCompactScoreD<-function(plan,distid, standardize=TRUE) {
    blocks<-which(plan==distid)
    if (length(blocks)==0) {
      return(1)
    }
    basemap<-basem(plan)
	bb<-getBbox(basemap,blocks)
   
	lw <- (bb$boxMax[1]-bb$boxMin[1])/(bb$boxMax[2]-bb$boxMin[2])
    if (lw>1) {lw <- 1/lw}
    score<-1-lw
    return(score)
}

#########################################################################
#
#  calcPACompactScoreD
#
# Returns the PA compactness as determined by the perimeter area ratio
#
#  See calcLWCompactScore
#
#########################################################################

CIRCRATIO<-sqrt(pi)/(2*pi)
calcPACompactScoreD<-function(plan,distid, standardize=TRUE) {
    blocks<-which(plan==distid)
    if (length(blocks)==0) {
      return(1)
    }
    basemap<-basem(plan)

    
    tmpinfo<-getDistrictShapeInfo(basemap,blocks)
    score<-sqrt(tmpinfo$area)/tmpinfo$perim
    if (standardize) {
    	score<-score/CIRCRATIO
    }
    score<-1-score
    return(score)
}


#########################################################################
#
#  calcSpatialHolesScoreD
#
# Returns score based on number of holes (topological complexity -- donut districts)
#
#  See calcSpatialHoleScore
#
#########################################################################

calcSpatialHolesScoreD<-function(plan,distid, standardize=TRUE) {
    blocks<-which(plan==distid)
    if (length(blocks)==0) {
      return(1)
    }
    basemap<-basem(plan)

    
    tmpinfo<-getDistrictShapeInfo(basemap,blocks,needShape=TRUE)
    score <- tmpinfo$numholes
    if (standardize) {
    	score<- (1- (1/(score+1)))
    }
    return(score)
}


#########################################################################
#
#  calcIneqScoreD
#
# Returns inequality of variable
#
#########################################################################

calcIneqScoreD<-function(plan,distid,eqvar,weightVar=NULL,type="Gini",parameter=NULL,...) {
    blocks<-which(plan==distid) 
    if (length(blocks)==0) {
      return(1)
    }
    
    df<-basem(plan)$df
    
    # don't send in ints -- these overflow
    # type - var
    
    tvec<-as.numeric(df[[eqvar]][blocks])
    if (!is.null(weightVar)) {
    	tvec<-tvec*df[[weightVar]][blocks]
    }
    
    if (type=="var") {
    	n<-length(tvec)
    	score<-(n-1/n)*sd(tvec)/mean(tvec)
    } else {
    	score<-ineq::ineq(tvec,parameter=parameter,type=type)
    }
    	
    return(score)
}

#########################################################################
#
#  calcGroupScore
#
# Returns number of groups split by a district. See calcGroupScore
#
#########################################################################

calcGroupScoreD<-function(plan,distid,groups=list(),penalties=1) {
    blocks<-which(plan==distid) 
    if (length(blocks)==0) {
      return(1)
    }

    isGroupSplit<-function(group) {
      tmp <- sum(group %in% blocks)
      return( !((tmp==0) || (tmp==length(group))))
    }
    
    penalties <- numeric(length(groups))+penalties
    splitgroups<-sapply(groups,isGroupSplit)
    score <- sum(splitgroups*penalties/sum(penalties))
    return(score)
}

#########################################################################
#
# calcREOCKCompactnessScoreD
#
# Returns the REOCK  compactness scores of each district.
# Each compactness score is determined by dividing the total area of the
# individual district by the area of the minimum bounding circle which
# circumscribes said district.
#
#########################################################################

"calcReockScoreD"<-function(plan,distid,usebb=FALSE, standardize=TRUE) {

  blocks<-which(plan==distid)
  if (length(blocks)==0) {
      return(1)
  }
  basemap<-basem(plan)
  polys<-BARD:::basePolys(basemap)[blocks]
  distArea <-sum(unlist(sapply(polys, 
  function(x)sapply(x@Polygons,function(x)x@area))))
  
  if (usebb) {
  	 bb<-basemap$bboxs[,blocks]
  	 xs <- c(bb[1,],bb[3,])
  	 ys <- c(bb[2,],bb[4,])
   
  } else {
     ch1<-sapply(polys,
     function(x)sapply(x@Polygons,function(x)x@coords[chull(x@coords),],simplify=FALSE),simplify=FALSE)
     xs<-unlist(sapply(ch1,function(x)sapply(x,function(x)x[,1])))
     ys<-unlist(sapply(ch1,function(x)sapply(x,function(x)x[,2])))
  
  }
  chi2<-chull(xs,ys)

  xs<-xs[chi2]
  ys<-ys[chi2]

  
  
  circleArea <- miniball(cbind(xs,ys))$sqradius*pi
  score <- distArea/circleArea
  if (standardize) {
      score <- 1- score
  }
  return(score)
}


#########################################################################
#
#  calcMomentScoreD
#
# Returns weighted cost of distance to centers. See calcMomentScoreD
#
#########################################################################

calcMomentScoreD<-function(plan,distid,standardize,
		centers=NULL,weightVar=NULL,penaltyExp=2,normalize=TRUE) {
    blocks<-which(plan==distid) 
    if (length(blocks)==0) {
      return(1)
    }
    if (is.null(weightVar)) {
    	weights<-1
    }
	blockCenters<-getBardCentroids(plan,blocks)
	
	# use district centroid if not specified
	if (is.null(centers)) {
		distarea<-sapply(basePolys(basem(plan))[blocks],function(x)attr(x,"area"))
		centerXY<-apply(blockCenters,2,function(x)weighted.mean(x,distarea))
	} else {
		centerXY<-getBardCentroids(plan,centers[plan==distid])
	}

    distance <- sqrt(rowSums(t(t(blockCenters)-as.vector(centerXY))^2))
	
    
    if (is.null(weightVar)) { 
    	tweights<-replicate(length(distance),1)
    } else {
		tweights<-basem(plan)$df[[weightVar]][blocks]
    }

    score <- sum(tweights*(distance)^penaltyExp)
    if (normalize) {
    	    normweight <- sum(tweights) * sum(distance)^penaltyExp
    	    score<-score/normweight
    }
    if (standardize) {
		score <- 1-1/(score+1)
    }
    return(score)
}

#########################################################################
#
#  calcUniScoreD
#
# Returns a univariate function quantity for a district. Used as a building 
# block for other score functions.
#
# Additional arguments
#   - varid - id of variable in basemap to evaluate
#   - unifunc - function to use for evaluation
#
#########################################################################

calcUniScoreD<-function(plan,distid,varid,unifunc=sum) {
    blocks<-which(plan==distid)
    if (length(blocks)==0) {
      return(0)
    }
   basemap<-basem(plan)
    return(unifunc(basemap$df[[varid]][blocks]))
}


#
# getBbox
#
# Helper function to calculate bounding box on bard plan subsets
#


getBbox<-function(basemap,blocks=integer(0)) {
	retval<-list()
	if(length(blocks)==0) {
		blocks<-1:dim(basemap)[1]
	}
	
	bboxg<-basemap$bboxs
	
	bMin<-apply(bboxg[1:2,blocks,drop=F],1,min)
	bMax<-apply(bboxg[3:4,blocks,drop=F],1,max)
	retval$boxMin<-bMin
	retval$boxMax<-bMax
	return(retval)
}


getDistrictShapeInfo<-function(basemap,blocks=integer(0), needShape=F) {
	if(length(blocks)==0) {
		blocks<-dim(basemap)[1]
	}
	
	tmpsub <- baseShape(basemap)[blocks,1]
	bbox<-tmpsub@bbox
	
	tarea<-sum(basemap$areas[blocks])
	tperim<-NULL
	
	if (!is.null(basemap$sharedPerims)) {
	  tperim<-sum(basemap$perims[blocks])
	  for (i in blocks) {
		for (j in seq(1,length.out=length(basemap$nb[[i]]))) {
			if (basemap$nb[[i]][j]<i) {next}
			
			if (any(blocks %in% basemap$nb[[i]][j])) {
				tperim<-tperim - basemap$sharedPerims[[i]][j]
			}
		}
	  }
	}
	  
	if (needShape || is.null(tperim)) {
		usub<-myUnionSpatialPolygons(tmpsub,replicate(length(blocks),1))
		tcent<-coordinates(usub)
		numholes<-sum(sapply(usub@polygons[[1]]@Polygons,function(x) slot(x, "hole")))
		shape<-usub
		tperim<-genPerim(usub@polygons,longlat=basemap$longlat)
	} else {
		numholes <- NA
		tcent<-c(NA,NA)
		shape<-NA
	}
	

	retval<-list( bbox=bbox, area=tarea,centroid=tcent, perim=tperim, shape=shape, numholes=numholes) 
	return(retval)
}

dynamicScoreSetup<-function(plan,lastscore,changelist) {
  if(is.districtonly(plan)) {
  	distids<-1
  	if (is.null(lastscore)) {
  		score <-0 
  	} else {
  		score<-lastscore
  	}
  } else if(is.null(lastscore)) {
    ndists<-attr(plan,"ndists")
    distids <- 1:ndists
    score <- numeric(ndists)
  } else {
    distids <- setdiff(unique(c(changelist[,2],plan[changelist[,1]])),NA)
    score <- lastscore
  }
  retval<-list()
  retval$distids<-distids
  retval$score<-score
  return(retval)
}

sameNeighbors<-function(plan,lastscore,changelist) {
  ndists<-attr(plan,"ndists")
  nb<-basem(plan)$nb  
  changedb<-changelist[,1]
  
  checkDists<-integer(0)
  
  #blocks that were next to others dis/reconnected
  for (i in seq(from=1,length.out=dim(changelist)[1])) {
  		x<-changelist[i,1]
  		vx<-plan[x]
  		ov<-changelist[i,2]
  		nl<-neighbors(nb,x)
	  	if (any(plan[nl]==vx) != any(plan[nl]==ov)) {
	  		checkDists<-c(checkDists,vx)
	  		checkDists<-c(checkDists,ov)
	  	}
  }
  
  distids<-setdiff(unique(c(plan[changelist[,1]],changelist[,2])),checkDists)

  if (length(distids)>0) {	
    nn <-setdiff(neighbors(nb,distids),changedb)
    nn2 <-setdiff(neighbors(nb,nn),changedb)
    nnv <- plan[nn2]
    
    # neighbors still connected?
    checkn<-sapply(distids,function(x){
  	  i <- (nnv==x)
  	  if (length(i)==0) { return(0)} 
  	ivec <- logical(length(plan))
  	ivec[nn2[i]]<-TRUE
  	n.comp.include(nb,ivec)$nc>1
  	})
  	checkDists<-c(checkDists,distids[checkn])
  }
  # check for single
  if(is.districtonly(plan)) {
  		checkDists<-intersect(1,checkDists)
  }
  return(checkDists)
}



##############################################################################
#          Testing Functions
###############################################################################

  testIncScore<-function(plan,n,FUN,...) {
  
    switches<-sample(length(plan),n)  
    switches2<-sample(length(plan),n)
    nplan<-plan
    nplan[switches]<-plan[switches2]
    cl<-cbind(switches,plan[switches])
    
    lsc<-FUN(plan,...)
    if (sum(FUN(nplan,lastscore=lsc,changelist=cl,...)!= FUN(nplan,...))>0) {
      cat("Failed:\n\n")
      print(cl)
      print(plan[switches2])
      return(FALSE)
    }
    return(TRUE)
  }
  
 
getPlanSeats<-function(plan) {
	if (is.null(attr(plan,"nseats"))) {
		retval<-attr(plan,"ndists")
	} else {
		retval <-attr(plan,"nseats")
	}
	return(retval)
}

getPlanMagnitudes<-function(plan) {
	if (is.null(attr(plan,"magnitudes"))) {
		retval <- replicate (length(plan),1)
	} else {
		retval <- attr(plan,"magnitudes")
	}
	return(retval)
}
