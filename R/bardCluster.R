##############################################################################
#
# bard clustering support
#
# 
# This module contains methods to use "snow" clusters
#
# The active cluster definition is stored in a closure (effectively, a package-level 
# variable.
#
# The workhorse function is lapplyBardCluster, this
# applies a function across a cluster if available, falling
# back to the local system if necessary. This will also retry any
# of the clustered operations that failed.
#
#############################################################################


#################################
#
# startBardCluster
#
#  This will attempt to initialize a SNOW cluster for use with
#  profilePlans, samplePlans, createGenoudPlan, etc.
#
#  If given a vector of character names, it attempts to create a snow SOCKET
#  cluster. Otherwise it uses the cluster object handed to it.
#
#  Each system in the cluster should have the BARD library installed. If 
#  bard is not installed, startBardCluster will attempt to install it.
#
# If an existing cluster exists, call stopBardCluster() first
#
# Arguments
#
#   - cl <- either:
#       - cluster object produced by makeCluster() or makeXXXXCluster() in snow
#       - character vector of machine names
#	- number of cores
#                          
# Returns
#    - returns logical indicating whether cluster was successfully initialized
#
# SEE R HELP FILE  FOR FULL DETAILS
# 
##################################

startBardCluster<-function(cl=0) {


   #
   # multicore case
   #

   if ((length(cl)==1) && is.numeric(cl)) {
       if (!mrequire("multicore",quietly=TRUE)) {
          warning("multicore is not available. Cannot run in parallel")
          return(FALSE)
        }
	if (cl==0) {
		cat("auto-determining number of cores...")
		cl<-multicore:::detectCores(all.tests=TRUE)
		if (is.na(cl)) {
			warning("Could not determine number of cores, defaulting to 2\n") 
			cl<-2
		} else {
			cat (paste("found ",cl," cores\n",sep=""))
		}	
	}
   	setBardCluster(newcl=cl)
	return(TRUE)
   }	

   if (length(cl)<2) {
      warning("Snow cluster size should be >= 2")
      return(FALSE)
   }

   #
   #snow case
   #

   if (!mrequire("snow",quietly=TRUE)) {
      warning("Snow is not available. Cannot cluster")
      return(FALSE)
   }
  # may be starting the cluster on a new node
  bardInstall<-function(bardRepos="http://cran.r-project.org") {
    if (mrequire("BARD",quietly=TRUE)) {
      return(TRUE)
    }
    repos<-getOption("repos")[1]
    if(is.null(repos) || repos=="@CRAN@") {
      repos <- bardRepos
    }
    install.packages("BARD",repos=repos,dependencies=c("Imports","Depends"))
      if (mrequire("BARD",quietly=TRUE)) {
      return(TRUE)
    }
  }  
   if (!is.null(setBardCluster())) {
    warning("Cluster already initialized. Need to stopBardCluster before initializing a new one")
    return(FALSE)
   }
   if(class(cl)=="character") {
      clnames<-cl
      cl<-makeSOCKcluster(clnames)
   }
   
   z<-try(parLapply(cl,replicate(length(cl),"http://cran.r-project.org"),bardInstall)
    ,silent=T)
   if (inherits(try(sum(unlist(z))),"try-error")) {
    warning("Cluster initialization failed -- can't load BARD on cluster nodes")
    return(FALSE)
   }
   
   setBardCluster(newcl=cl)
   return(TRUE)
}

#################################
#
# stopBardCluster
#
# This uses snow stopCluster()  on cluster and updated BARD cluster tracking.
# Will be called automatically when BARD module is unloaded
#
# Arguments
#
#                          
# Returns
#    - previously configured bard cluster
#
# SEE R HELP FILE  FOR FULL DETAILS
# 
##################################

stopBardCluster<-function() {
  cl<-setBardCluster()
  if (is.null(cl) || (length(cl)==1)) {
	# if no cluster or multicore cluster
    setBardCluster(NULL)
    return();
  }
  cat("*** Shutting down bard cluster ..\n\n")
  status<-stopCluster(cl)
  setBardCluster(NULL)
  return(status)
}
  
##############################################################################
#           INTERNAL MODULE FUNCTIONS -- DO NOT EXPORT
##############################################################################
  
#################################
#
# lapplyBardCluster
#
# Internal function.
#
# This is similar to snows() parLapply, which is similar to lapply().
# It attempts to distribute a standard lapply(X,FUN,...) across an existing
# bard cluster. 
# 
# This function is more conveneient than using parLapply directly for a few reasons:
# - load balancing is selectable with a flag
# - the defined bard cluster (startBardCluster) will be used if available,
#   if not, lapply() will be used automatically
# - snow clusters can be unreliable. Bard uses an automatic retry mechanism
#   to retry portions of the parLapply() that fail, and falls back to the local
#   lapply if necessary.
#
# Arguments
#   X - list
#   FUN - function
#  ... - additional arguments for FUN
#  LB - logical, use load balancing
#  maxRetries - number of retries on failed parLapply elements, before falling back
#         to lapply
#
#                          
# Returns
#    - previously configured bard cluster
#
# 
##################################

lapplyBardCluster<-function(X,FUN,...,LB=TRUE,maxRetries=3) {
  cl<-setBardCluster()
  
  if (length(X)==0) {
	return(list())
  }
  if (is.null(cl)) {
    retval<-lapply(X,FUN,...)
    return(retval)
  }
  
  if (length(cl)>1) {
	return (lapplyBardCluster.snow(X,FUN,...,LB=LB,maxRetries=maxRetries))
  } else {
	return (lapplyBardCluster.multicore(X,FUN,...,LB=LB))
  }
}


lapplyBardCluster.multicore<-function(X,FUN,...,LB=TRUE) {
  cl<-setBardCluster()
	mclapplySafer<- function(X,FUN,...){
		newFUN<-function(X,...){
			if(multicore:::isChild()) 	{multicore:::closeAll()}
			FUN(X,...)
		}
 		mclapply(X,newFUN,...)
	}
	return(mclapplySafer(X,FUN,...,mc.cores=cl,mc.silent=FALSE,mc.preschedule=!LB))
}

lapplyBardCluster.snow<-function(X,FUN,...,LB=TRUE,maxRetries=3) {

  cl<-setBardCluster()
  # snow logic...
  if (LB) {
    clusterFun <- clusterApplyLB
  } else {
    clusterFun <- parLapply
  }
  retval<-vector(length(X),mode="list")
  notdone<-1:length(X)
  
  # the manipulations of notdone  below are to avoid losing the try error class
  # if clusterFun itself fails, retval[notdone]<-clusterFun elements end up unclassed
  # char strings instead of try-error objects
  
  for (i in 1:maxRetries) {
    tmpres <- try(clusterFun(cl,X[notdone],FUN,...),silent=T)
    # check for failure of entire call
    if (inherits(tmpres,"try-error")) {
      warning("Cluster call failed")
      break
    }
    for (j in 1:length(notdone)) {
      retval[[notdone[j]]] <- tmpres[[j]]
    }
    # process failures of individual nodes
    notdone<-which(sapply(retval,class)=="try-error")
    if (length(notdone)==0) {
      break
   }
 }
  
  if(length(notdone)>0) { 
    warning("Cluster apply failed multiple retries") 
    retval[notdone]<-lapply(X[notdone],FUN,...)
  }
  return(retval)

}
  
  
#################################
#
# setBardCluster
#
#  This is an internal function. It uses a closure as a private
#  package-level variable to keep track of the currently defined
#  SNOW cluster
#
# Arguments
#  newcl   = either missing, a cluster object produced by makeCluster() in snow,
#            or a character vector of machine names
#          - if missing, returns current cluster assignment, does not change assignment
#                          
#
# Returns
#    - returns previously assigned cluster, or NULL if none available
#
# 
##################################


setBardCluster<-local ({
 curCluster<-NULL 
 function(newcl) {
    retval <- curCluster;
    if (!missing(newcl)) { 
        curCluster<<-newcl;
    }
    return(retval)
 } 
})

#################################
#
# bardClusterExport
#
#  This is an internal function equivalent to clusterExport
#  but with the bard retry framework
#
##################################


bardClusterExport<-function(list) {
    cl<-setBardCluster()
    if (is.null(cl)) {
	return(invisible())
    }
   
    if (length(cl)==1) {
	#multicore cluster, export is a no-op
	return(invisible())
    }
    gets <- function(n, v) {
        assign(n, v, env = .GlobalEnv)
        NULL
    }
    for (name in list) {
	lapplyBardCluster(
		replicate(length(cl),name),
		gets,
		get(name, env = .GlobalEnv),
		LB=FALSE
		)
    }
   invisible()
}
