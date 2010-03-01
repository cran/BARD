


##############################################################################
#
# bard initial plan generation
#
# 
# This module contains methods to generate initial plans. These
# plans are not guaranteed to have any specific properties, unless
# otherwise mentioned. They are used as inputs for the optimization
# refinements
#
# They all return a _plan_assignment_vector_, a bardPlan object,
# which is defined as follows:
#
#       Each assignment is an integer-value vector of length N, 
#       where N is the the number of blocks in the basemap. The 
#       value of each item in the vector is the district id in (1:N)
#       which that block is assigned.   Unassigned blocks should be set to NA.
#
#       The plan is also assigned attributes, including the number of districts
#       and the bard basemap from which it is generating.
#
# All plan generation functions take the following arguments:
#
# basemap -- a basemap as returned by bard.read
# ndists -- integer number of districts to create
#
# They may take additional arguments, but not all plan functions are
# guaranteed to accept them.
#
#
##############################################################################


#################################
#
# fillHolesPlan
#
#  This takes an input plan with holes, blocks that are not assigned
#  to districts, and assigns each of these blocks to an existing district
#
# Arguments
#  plan   <- input plan
#  method <- method used to fill holes                            
#
# Returns
#   plan assignment vector
#
#  See R documentation for full details.
# 
##################################

fillHolesPlan<-function(plan,method=c("random","fixed","closest"), fixed=1) {
  method <- match.arg(method)
  ndists<-attr(plan,"ndists")
  missblocks <- which(is.na(plan)) 
  
  getNeighbor<-function(i) {
    nbl<- neighbors(basem(plan)$nb,i)
    choice<-resample(nbl,1)
    if (length(choice)==0) {
      return(NA)
    } else {
      return(plan[choice])
    }
  }
  
  switch(method,
         random = plan[missblocks] <- floor(1+ndists*runif(length(missblocks))),
         fixed = plan[missblocks]  <- fixed,
         closest = {
           if(length(missblocks) == length(plan)) {
              warning("All blocks are missing, assigning to fixed")
              plan[missblocks]=fixed
           } else {
            while(length(missblocks)>0) {
               plan[missblocks]<-sapply(missblocks,getNeighbor)
               missblocks <- which(is.na(plan))
            }
           }
        }       
  )
 return(plan)
}


#################################
#
# createAssignedPlan
#
# Retrieves a plan indexed in an existing variable.
#
# Arguments
#   basemap 
#   id.var                                c
#
# Returns
#   plan assignment vector
#
#
##################################

createAssignedPlan<-function(basemap,predvar="BARDPlanID") {
  nblocks<-length(basemap$polys)
   if ((length(predvar)==1) &&  is.character(predvar)) {
      tmpplan<-basemap$df[[predvar]]
      if (is.null(tmpplan)) {
          warning(paste("No variable in basemap with name", predvar))
          return(NULL)
      }
   } else {
     tmpplan <- integer(nblocks)+predvar
  }
    
  # convert to continuous ordered integers
  plan<-as.numeric(factor(tmpplan,levels=sort(unique(tmpplan))))
  
  if (!identical(as.numeric(sort(unique(plan))), sort(unique(tmpplan)))) {
    warning("District identifiers were not continuous and were reordered")
  }
  
  ndists<-length(unique(na.exclude(plan)))
    
  if (sum(is.na(plan))>0) {
    warning("Some blocks are not assigned. Use fillHolesPlan.")
    ndists<- ndists-1
  }
 
  attr(plan,"ndists")<-ndists
  basem(plan)<-basemap
  class(plan)<-"bardPlan"
  return(plan)
}
                                    


#################################
#
# createRandomPlan
#
# Generate a plan completely at random.
# The plan is completely assigned, but not contiguous.
#
# Arguments
#   basemap 
#   ndists
#
# Returns
#   plan assignment vector
#
#
##################################

createRandomPlan<-function(basemap,ndists) {
  nblocks<-length(basemap$polys)
  plan<-ceiling(runif(nblocks)*ndists)
  attr(plan,"ndists")<-ndists
  basem(plan)<-basemap
  class(plan)<-"bardPlan"
  return(plan)
}

createRandomPopPlan<-function(basemap,ndists,
  predvar="POP" ) {
  
  # syntactic sugar -- revert to create random plan
  if (is.null(predvar)) {
    return(createRandomPlan(basemap,ndists))
  }
  
  totPop <- sum(basemap$df[[predvar]])
  maxPop <- (totPop/ndists) 
  popDist <- integer(ndists)
  
  plan <- integer(length(basemap$polys))
  is.na(plan)<-TRUE
  
  while (length(blocksleft<-which(is.na(plan)))>0) {
    choice<-resample(blocksleft,1)
    blockpop<-basemap$df[[predvar]][choice]
    canDist <- which((popDist+blockpop) <maxPop)
    if (length(canDist)==0) {
      canDist <- 1:ndists
    }
    distid<-resample(canDist,1)
    plan[choice]<-distid
    popDist[distid]<-popDist[distid] + blockpop
  }
  attr(plan,"ndists")<-ndists
  basem(plan)<-basemap
  class(plan)<-"bardPlan"
  return(plan)
}

#################################
#
# createKmeansPlan
#
# Generate a plan via kmeans of geographic centroids.
# The resulting plan is usually nearly
# contiguous and vaguely compact. Neither
# is guaranteed.
#
# Arguments
#   basemap 
#   ndists
#   ...  arguments to pass to kmeans
#
# Returns
#   plan assignment vector
#
##################################

createKmeansPlan<-function(basemap,ndists) {
  dist.centroids<-getBardCentroids(basemap)

  plan <-  kmeans(dist.centroids,ndists)$cluster
  attr(plan,"ndists")<-ndists
  basem(plan)<-basemap
  class(plan)<-"bardPlan"
  return(plan)
}


#################################
#
# createWeightedKmeansPlan
#
# Generate a plan via kmeans of geographic centroids, weighted
# by another variable. 
#
# The resulting plan is usually nearly
# contiguous and vaguely compac. Neither
# is guaranteed
#
# See R documentation for more information
#
# Arguments
#   basemap 
#   ndists
#   centers
#   trimfactor
#   smallBlock
#
# Returns
#   plan assignment vector
#
##################################

createWeightedKmeansPlan<-function(basemap,ndists,centers=c(),weightVar=NULL,trimfactor=2.5,
	smallBlock=c("cap","closest")) {

  smallBlock<-match.arg(smallBlock)
  trimmedWeight<-function(tv) {
		popadj<- (10^max(0,floor(log10(max(tv)-min(tv)))-trimfactor))
		res<-round(tv/popadj)
		return(res)
	}
	
 if (is.null(weightVar)) {
    tw<-1 
  } else if (!any(names(basemap$df)==weightVar)) {

  	warning("Weight variable does not exist")
	tw<-1
  } else {
	tw<-trimmedWeight(basemap$df[[weightVar]])
  }
  
  
  # deal with 0 weighted blocks simply
  if (smallBlock != "closest") {
	tw[which(tw==0)] <- 1
  }
  
  dist.centroids<-getBardCentroids(basemap)
  if (length(centers)==0) {
	kmcenters<-ndists
  } else if(length(centers)!=ndists) {
	warning("Number of centers must be the same as number of districts")
	kmcenters<-ndists
  } else {
	kmcenters<-dist.centroids[centers,]
  }
	
  dist.centroids <- cbind(dist.centroids,1:nrow(dist.centroids))
  dist.centroids <- dist.centroids[rep(1:nrow(dist.centroids),tw),]
  tmpplan <-  kmeans(dist.centroids[,1:2],centers<-kmcenters)$cluster
  tmpplan <- aggregate(tmpplan,by=list(dist.centroids[,3]),FUN=function(x)round(mean(x)))
  plan<-vector(length=length(tw))
  plan[tmpplan[[1]]]<-tmpplan[[2]]
  attr(plan,"ndists")<-ndists
  basem(plan)<-basemap
  class(plan)<-"bardPlan"
  if (smallBlock == "closest") {
    is.na(plan[which(plan==0)])<-TRUE
	plan <- fillHolesPlan(plan,method="closest")
  }
  return(plan)
}

#########
## sampleDistricts 
##
##
## Convenience function
##
## Repeatedly calls districtonly district creation algorithm.
##
#################

quickSampleDistricts<-function(ngenplans, basemap, ndists, distFUN=createContiguousDistrict, ...) {
	
	retval <- replicate(ngenplans,distFUN(basemap,ndists,...),simplify=FALSE)
	class(retval)<-"bardDistSample"
	return(retval)
}



#################################
#
# CDOcontiguityPlan
#
# Generate a plan via the "coniguity algorithm"
# described in:
#
# Cirincione et. al. (2000). Political Geography  19: 189-211
#
# The plan is completely assigned, contiguous and equal in population
#                                                 
# Arguments
#   basemap                                                    
#   ndists
#   predvar = name of variable to use for assessing population
#   threshold = acceptable percentage deviation
#   ssize - not included in CDO originally (leave at 1 for original)
#           check up to ssize neighbors at random, choosing the one
#           with the fewest foreign neighbors
#   usebb - use bounding box based compactness approach described by CDO
#
# Returns
#   plan assignment vector
#
# Details:
# 
# As described on 196:
#
# The first algorithm, the contiguity algorithm, begins by
# randomly selecting a block group to serve as the “base” of
# the first district. It then constructs a “perimeter list”
# containing the unassigned block groups contiguous to the base block group.
# The program then randomly selects a block group from the perimeter 
# list to add to the emerging district and adjusts the perimeter list. 
# This process continues until the population of the emerging district
# achieves the desired district population level. (A newly 
# created district is thrown out if its population deviates by more than 1%
# from the ideal district average population, which in this case is 581,117.)
# The next district begins with the random selection of a census block group
# from among those that touch one of the completed districts.
#
##################################


#
# Convenience wrapper
#

createContiguousDistrict<-function( basemap,ndists, predvar="POP",
  threshold = .05, ssize=20,
  usebb=TRUE,
  maxtries=(10/threshold),
  neighborstarts=TRUE,
fillholes=TRUE) {

	return(createContiguousPlan(basemap=basemap,
				    ndists=ndists,
				    predvar=predvar,
				    threshold=threshold,
				    ssize=ssize,
				    usebb=usebb,
				    maxtries=maxtries,
				    neighborstarts=neighborstarts,
				    fillholes=fillholes,
				    districtonly=TRUE))
}

createContiguousPlan<-function(basemap,ndists,
  predvar="POP",
  threshold = .05, ssize=20,
  usebb=TRUE,
  maxtries=(10/threshold),
  neighborstarts=TRUE,
  fillholes=TRUE,
  districtonly=FALSE) {
    DEBUG<-FALSE
    maxtries<-max(1,maxtries)

    
    for (i in 1:maxtries) {
      result<-CDOInner(
           basemap=basemap,ndists=ndists,predvar="POP",
           threshold=threshold, DEBUG=DEBUG,ssize=ssize, usebb=usebb,
           neighborstarts=neighborstarts,fillholes=fillholes,
           districtonly=districtonly)

      if (result$success) {
        break
      }
      if (DEBUG) {
         print(paste("************** retrying plan: ", i))
         flush.console()
      }
    }
    if(districtonly) {
    	attr(result$plan,"districtonly")<-TRUE
    }
    return(result$plan)
}

CDOInner<-function(basemap,ndists,
  predvar,
  threshold,
  DEBUG,
  ssize,
  usebb,
  neighborstarts,
  fillholes,
  districtonly) {
  
  # setup  for plan
  blocklist <- (1:length(basemap$polys)) * 0   # list of block ids in plan
  nblocks <- length(blocklist)                 # number of blocks
  totpop <- sum(basemap$df[[predvar]]) # total population
  targetpop <- totpop/ndists
  targetlow <-  (1-threshold)*targetpop    #population targets
  targethigh <- (1+threshold)*targetpop
  MAXSTARTS <- max(8,(.1/threshold))            # Max restarts 
  tmppop <- basemap$df[[predvar]]  # vector of block populations
  
  # plan object 
  attr(blocklist,"ndists")<-ndists
  basem(blocklist)<-basemap
  class(blocklist)<-"bardPlan"
     
  # iterate over districts     '
  if (districtonly) {
     ndistcount<-1
     innertargetpop <- targetpop
  } else if (fillholes) {
    ndistcount <- ndists
    innertargetpop <- (1-threshold/ndists)*targetpop
  } else {
    ndistcount<-ndists-1
    innertargetpop <- targetpop
  }
  for (i in 1:ndistcount) {  
   isdone<-FALSE   
   nstarts<-1  
   while ( !isdone && (nstarts<=MAXSTARTS) )  # restart if no contiguous move
    {
      # start from neighbors of existing district

       if (neighborstarts && i >1) {
         tmpnb <- neighbors(basemap$nb,which(blocklist>0))
         tmpnb <- tmpnb[which(blocklist[tmpnb]==0)]
         tmpstart<-sample(tmpnb,1)
       } else {
         tmpstart<-sample(which(blocklist==0),1)
       }
      
      # reset district population and blocklist
      blocklist[tmpstart]<-i
      
      # set initial tracking params
      curpop <- tmppop[tmpstart]
      curbbox <- unlist(getBbox(basemap,tmpstart))
      curnb <- neighbors(basemap$nb,tmpstart)     #neighbors list
      curnb <- curnb[which(blocklist[curnb]==0)]
      
      # CORE LOOP: add neighbors at random to current districts
      
      while ( (curpop < innertargetpop) && (length(curnb)>0))  {

           if (DEBUG>1) { 
             print(paste("dist",i,"curpop",curpop))
             flush.console()
           }

           # select a block from neighbors
           newblockIndexes<-NULL
           
           if (usebb) {
             # check blocks to see if they fall in the bounding box
             tmpbblist <- 
             	sapply(curnb,function(x)unlist(getBbox(basemap,x)))
             bbcurnb <- curnb[
              which(sapply(tmpbblist,function(x)(x[1]>=curbbox[1] & x[2]>=curbbox[2] 
               & x[3]<=curbbox[3] & x[4]<=curbbox[4])))]
             if (length(bbcurnb>0)) {
               newblockIndexes <- resample(bbcurnb,min(ssize,length(bbcurnb)))
             } 
           } 
           
           # either no usebb or fallthrough because no blocks available in bbox 
           if (is.null(newblockIndexes)) { 
              newblockIndexes <- resample(curnb,min(ssize,length(curnb)))
           }  
           
           if (ssize>1) {
              tmpIndex <- sapply(newblockIndexes,function(x){
                xnb<-neighbors(basemap$nb,x)
                sum(blocklist[xnb]==i)*10000 + 100*sum(blocklist[xnb]>0) - 10* sum(blocklist[xnb]==0)
              })
              tmpi <- which.max(tmpIndex) 
           } else {
              tmpi<-1
           }
           
           newblock <- newblockIndexes[tmpi]
           blocklist[newblock]<-i
           
           # dynamic update of pop, bbox, neighborhood
           curpop <- curpop + tmppop[newblock]
          if (usebb) {
              tmpbb <- unlist(getBbox(basemap,newblock))
              curbbox[1] <- min(curbbox[1],tmpbb[1]); curbbox[2] <- min(curbbox[2],tmpbb[2])
              curbbox[3] <- max(curbbox[3],tmpbb[3]); curbbox[4] <- max(curbbox[4],tmpbb[4])
           }
           #update neighborhood list
           tmpnb <- neighbors(basemap$nb,newblock)
           tmpnb <- tmpnb[which(blocklist[tmpnb]==0)]
           curnb <- unique( c(tmpnb, setdiff(curnb,newblock)))
                           
                
      }  # end neighbor grow loop
      
      # restart check
      if (DEBUG) {
        if (nstarts==MAXSTARTS) {
            print("MAXSTARTS EXCEEDED")
            flush.console()
        } 
        if (DEBUG>1) {
            plot(blocklist)
        }
            curnbn <- neighbors(basemap$nb,which(blocklist==i))
            curnbn <- curnbn[which(blocklist[curnbn]==0)]
            curpopn <- sum(tmppop[which(blocklist==i)])
            print(paste("Restart check","dist",i, "nstarts", nstarts,"maxstarts", MAXSTARTS,
             curpop, targetlow,targethigh))
            if (any(curpopn!=curpop)) {
                print (curpop)
                print (curpopn)
                stop("failed consistency check on dynamic population")
            }
            if (any(sort(curnbn)!=sort(curnb))) {
               print("current")
               print (curnb)
                  print("check")
               print (curnbn)
               stop("failed consistency check on dynamic neighbor")
            }
            if (usebb) {
              #tmpBB<- #sapply(basemap$polys[blocklist==i],function(x)attr(x,"bbox"))
              #curbboxn<-c(apply(tmpBB[1:2,,drop=F],1,min),apply(tmpBB[3:4,,drop=F],1,max))
             curbboxn<-unlist(getBbox(basemap,blocklist))
             if (any(curbbox!=curbboxn)) {
               print("current")
               print (curbbox)
                  print("check")
               print (curbboxn)
               stop("failed consistency check on dynamic bbox")
            }
          }
            
           if (length(curnb)==0) {
             print("NO NEIGHBORS")
           } 
           flush.console()
           if (DEBUG>2) {
           	Sys.sleep(2)
           }
      } 

      if ((nstarts<MAXSTARTS) && ((curpop<targetlow) || (curpop>targethigh)) ) {
         nstarts <- nstarts + 1
         if (DEBUG) {
            print("RESTARTING")
            flush.console()
         }
         blocklist[which(blocklist==i)] <- 0
      } else {
        isdone<-TRUE
      }
    }    # end restart loop
  }      # end district count
  
  # assign final district
  if (districtonly) {
    blocklist[which(blocklist==0)] <- 2
  } else if (fillholes) {
    is.na(blocklist[which(blocklist==0)])<-TRUE
    blocklist<-fillHolesPlan(blocklist,method="closest")
  } else {
    blocklist[which(blocklist==0)] <- ndists
  }
  
  # checks on final plan
  if (DEBUG) {
           plot(blocklist)
           Sys.sleep(5)
  }
  if (i<ndists && !districtonly ) {
          if (DEBUG) {
            print("PLAN INCOMPLETE")
            flush.console()
         }
    success<-FALSE
  } else  { 
     if (!districtonly && !fillholes && calcContiguityScoreD(blocklist,distid=5)!=0) {
       if (DEBUG) {
            print("LAST DISTRICT NOT CONTIGUOUS")
            flush.console()
         }
        success<-FALSE
    } else {  
       if (districtonly) {
       	distpops <- sum(tmppop[blocklist==1])
       	} else {
       	distpops <- sapply(1:ndists, function(x)sum(tmppop[blocklist==x]))
       }
       
       if (any(distpops<targetlow | distpops>targethigh)) {
         if (DEBUG) {
            print("FINAL DISTRICTS OUT OF POPULATION TARGET RANGE")
            print (paste("low",targetlow,"high",targethigh))
            print (distpops)
            flush.console()
         }
        success<-FALSE
       }  else {
          if (DEBUG) {
            print("SUCCESS!")
            flush.console()
         }
        success<-TRUE
     }
    }
  }
 
  return(list(plan=blocklist,success=success))
}


###
###  Generic methods
###

summary.bardPlan<-function(object,...) {
  tmpplan<-object
  if (is.districtonly(object)) {
  	tmpplan<-factor(tmpplan,labels=c("district","unassigned"))
  }
  retval<-table(tmpplan,dnn=list("Number of blocks in each district"))
  class(retval)<-"bardPlan.summary"
  return(retval)
}

print.bardPlan.summary<-function(x,...) {
  class(x)<-"table"
  print(x,...)
}

print.bardPlan<-function(x,...) {
  ndists<-attr(x,"ndists")
  if (is.districtonly(x)) {
  	ndists<-1
  	cat("Single-district  -- only first district analyzed\n\n")
  }
  
  for (i in 1:ndists) {
    cat("\n\nBlocks in district ",i,":\n\n",...)
    print(which(x==i),...)
  }
  if (sum(is.na(x))>0) {
    cat("\n\nHoles :\n\n",...)
    print(which(is.na(x)))
  }
}

plot.bardPlan.summary<-function(x,...) {
  class(x)<-"table"
  pie(x,main="number of blocks in each district",...)
}


#################################
#
# plotPlan
#
# Simple functions to display map of district plan
#
#
# Arguments
#
# plan - plan allocation 
# basemap - basemap
# ndists - override number of districs
# changed - index array of units changed, plots incrementally only
# newPlot - refreshes plot
#
#################################
"plot.bardPlan" <-
function(x,basemap=NULL,ndists=NULL, changed=NULL, newPlot=TRUE,col=NULL,...) {
  if(is.null(basemap)) {
 
    basemap<-basem(x)
  }
  if (is.null(basemap) || is.na(basemap) ) {
    plot(matrix(x), ... )
    return(invisible(TRUE))
  }
  mappolys <- basemap$polys
  if (is.null(ndists)) {
      ndists <- attr(x,"ndists")
  }
  if (is.null(ndists)) {
      ndists <- length(unique(x))
  }
	if (is.null(col)) {
		colors<-topo.colors(ndists)
	} else {
		colors <- col

        }
	if (newPlot) {
                plot(basemap$shape, col="white", ...)
        }
        for (i in 1:ndists) {
                if (is.null(changed)) {
                        tmp <- x==i;
                } else {
                        ti <- which(x[changed]==i);
                        tmp <- logical(length(x));
                        tmp[changed[ti]]<-TRUE;
                }

                if (length(which(tmp)) > 0 ) {
                        submap <- basemap$shape[which(tmp),]           	
                       plot (submap, add=T, col=colors[i])
                }
        }
 return(invisible(TRUE))
}

#
#  basem methods
#
#  More reference hacking. Copying basemap to an attribute makes a deep copy
#  according to gc, assigning to an existing list element does not. Hence
#  these methods to store basemaps, which may be large, in a memory efficient way
#
# Using environments is slower , but the time is not proportional
# to the size of the basemap, and is negligible given the length of cost
# function evaluations. (On my system it was 1000th of an eval, or approx 4.8
# secs more per 100000 iterations than using attr() alone)
#


basem <- function(object,...) UseMethod("basem")
basem.default <- function(object,...) {
  return(get("content",envir=attr(object,"basemap")))
}

"basem<-"<-function(object,...,value) UseMethod("basem<-")
"basem<-.default" <- function(object,...,value) {
      if (!is.null(value) && !inherits(value,"bardBasemap")) {
        warning("Supplied basemap is of the wrong class")
      }
      attr(object,"basemap")<-new.env()
      assign("content",value,envir=attr(object, "basemap"))
      return(object) 
}

#
# checkPlans
# 
# Check a list of plans for mutual consistency
#

checkPlans<-function(plans) {
    
    if (!is.list(plans)) {
      plans <- list(plans)
    }
    retval<-TRUE
    checkclass<-
      sapply(plans,function(x)inherits(x,"bardPlan"))
      
    checkdistrictonly<-
    sapply(plans,function(x)is.districtonly(x)==is.districtonly(plans[[1]])) 
    
    if (!all(checkdistrictonly)) {
      warning("Plans and districts cannot be mixed")
      retval<-FALSE
    }
  
    if (!all(checkclass)) {
      warning("Plans must have class bardPlan")
      retval<-FALSE
    }
    checklengths <- 
      sapply(plans,function(x){attr(x,"ndists")==attr(plans[[1]],"ndists")})
    if (!all(checklengths)) {
      warning("Plans must have same number of districts")
      retval<-FALSE
    }
    checkbase <- 
      sapply(plans,function(x){basem(x)==basem(plans[[1]])})
    if (!all(checkbase)) {
      warning("Plans must same basemap")
      retval<-FALSE
    }
    return(retval)
}

#
# getBardCentroids
#
# helper function to extract centroids
#

getBardCentroids<-function(x,i=NULL) {
  if (class(x)=="bardPlan") {
	basemap<-basem(x)
  } else {
	basemap<-x
  }
  if (is.null(i)) i<-1:length(basemap$polys)
  dist.centroids<-coordinates(basemap$shape[i,])
  return(dist.centroids)
}

is.districtonly<-function(plan) {
  districtonly<-attr(plan,"districtonly")
  retval<-(!is.null(districtonly) && (districtonly))
  return(retval)
 }
	
