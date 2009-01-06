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

  nholes<-sum(is.na(plan))
  if(standardize) {
    score <- nholes/length(plan)
  } else {
    score <- nholes
  }
  return(score)
}

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

  if(is.null(lastscore)) {
    ndists<-attr(plan,"ndists")
    distids <- 1:ndists
    score <- numeric(ndists)
  } else {
    distids <- setdiff(unique(c(changelist[,2],plan[changelist[,1]])),NA)
    score <- lastscore
  }
  
  score[distids]<- 
    sapply(distids,function(x)calcContiguityScoreD(plan,x,standardize))
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

  if(is.null(lastscore)) {
    ndists<-attr(plan,"ndists")
    distids <- 1:ndists
    score <- numeric(ndists)
  } else {
    distids <- setdiff(unique(c(changelist[,2],plan[changelist[,1]])),NA)
    score <- lastscore
  }
    score[distids]<-sapply(distids,function(x)
      calcLWCompactScoreD(plan,x,standardize))
    return(score)
}

calcReockScore<-function(plan,
  lastscore=NULL, changelist=NULL, standardize=TRUE ) {

  if(is.null(lastscore)) {
    ndists<-attr(plan,"ndists")
    distids <- 1:ndists
    score <- numeric(ndists)
  } else {
    distids <- setdiff(unique(c(changelist[,2],plan[changelist[,1]])),NA)
    score <- lastscore
  }
    score[distids]<-sapply(distids,function(x)
      calcReockScoreD(plan,x,standardize))
    return(score)
}
  
#########################################################################
#
# calcPopScore
# 
# Returns a score based on teh population equality of the districts.
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
  if(is.null(lastscore) || is.null(attr(lastscore,"rawscore"))) { 
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
      
      score<- sqrt(abs((rawscore-sum(rawscore)/ndists))/sum(rawscore))
  } else {
      score<-rawscore
  }
  attr(score,"rawscore")<-rawscore
  return(score)
}

#########################################################################
#
# calcRangeScoree
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
  targrange=c(.65,.70),
  lastscore=NULL, changelist=NULL, standardize=TRUE ) {
  
  if ((length(targrange)!=2) || min(targrange<0) || max(targrange>1) || 
    (targrange[1]>targrange[2])) {
    stop("Illegal range")
  }

  ndists<-attr(plan,"ndists")
  basemap<-basem(plan)
  if(is.null(lastscore) || is.null(attr(lastscore,"rawscore"))) { 
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
  
  if (standardize) {
      # penalty function for being outside range  
      ratios<-(rawscore1/(rawscore1+rawscore2))
      score<-pmin(abs(ratios - targrange[2]),abs(ratios-targrange[1]))
      score[ratios >= targrange[1] & ratios <=targrange[2]]<-0
      score[which(is.nan(ratios))]<-1
  } else {
      #report range
      score<-rawscore1/(rawscore1+rawscore2)
  }
  attr(score,"rawscore1")<-rawscore1
   attr(score,"rawscore2")<-rawscore2
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

 if(is.null(lastscore)) {
    ndists<-attr(plan,"ndists")
    distids <- 1:ndists
    score <- numeric(ndists)
  } else {
    distids <- setdiff(unique(c(changelist[,2],plan[changelist[,1]])),NA)
    score <- lastscore
  }
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

calcMomentScore<-function(plan,standardize=TRUE,centers=NULL,weightVar=NULL,penaltyExp=2,
           lastscore=NULL, changelist=NULL ) { 

 if(is.null(lastscore)) {
    ndists<-attr(plan,"ndists")
    distids <- 1:ndists
    score <- numeric(ndists)
  } else {
    distids <- setdiff(unique(c(changelist[,2],plan[changelist[,1]])),NA)
    score <- lastscore
  }
  
 if ((!is.null(centers)) &&length(centers)!=ndists) {
  warning("wrong number of centers specified")
  return(NA)
 }
 if  (!is.null(weightVar) && is.null(basem(plan)$df[weightVar])) {
 	  warning("nonexistent weight variable  specified")
	  return(NA)
 }
    score[distids]<-sapply(distids,function(x)
      calcMomentScoreD(plan,x,standardize,centers,weightVar,penaltyExp))
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
	
	retval<-distcombfun(sapply(scores,scorecombfun))
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
# calculated with the total areas added together and returned by the
# function.
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

"calcReockScoreD"<-function(plan,distid,standardize=TRUE) {

  blocks<-which(plan==distid)
  if (length(blocks)==0) {
      return(1)
  }
  basemap<-basem(plan)
  polys<-basemap$polys[blocks]
  distArea <- 
    sum(sapply(polys,function(x)attr(x,"area")))
    
  xs<-unlist(sapply(polys,function(x)x[,1]))
  ys<-unlist(sapply(polys,function(x)x[,2]))
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
		centers=NULL,weightVar=NULL,penaltyExp=2) {
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
		distarea<-sapply(basem(plan)$polys[blocks],function(x)attr(x,"area"))
		centerXY<-apply(blockCenters,2,function(x)weighted.mean(x,distarea))
	} else {
		centerXY<-getBardCentroids(plan,centers[distid])
	}

    distance <- sqrt(rowSums(t(t(blockCenters)-as.vector(centerXY))^2))
	
    
    if (is.null(weightVar)) { 
    	tweights<-1
	} else {
		tweights<-basem(plan)$df[[weightVar]][blocks]
    }
	if (standardize) {
		tweights<-tweights/sum(tweights)*length(tweights)
	}

    score <- sum((distance*tweights)^penaltyExp)
    if (standardize) {
		score = 1-1/(score+1)
	}
	return(score)
}

#
# getBbox
#
# Helper function to calculate bounding box on bard plan subsets
#


getBbox<-function(basemap,blocks) {
	tmpBB<- sapply(basemap$polys[blocks],function(x)attr(x,"bbox"))
    boxMax<- apply(tmpBB[3:4,,drop=FALSE],1,max)
    boxMin<- apply(tmpBB[1:2,,drop=FALSE],1,min)
	retval<-list()
	retval$boxMin<-boxMin
	retval$boxMax<-boxMax
	return(retval)
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
  
 
