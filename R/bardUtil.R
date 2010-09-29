##############################################################################
#
# bard utility functions
#
# 
# Miscellaneous internal utility functions
#
#
##############################################################################

##################################################
#
# resample
#
# resample safely, based on sample() help
# 
# Arguments:
#     x - vector (scalar will be treated as vectof length 1)
#     size - size of sample
#     ... - other arguments for sample()
#
# Returns: sample elements from vector
#
##################################################

 resample<- function(x, size, ...) {
 if(length(x) <= 1) {
		if(!missing(size) && size == 0) {
			return(x[FALSE]);
		} else {
			return(x);
		}
  	} else {
		return(sample(x, size, ...));
	}
 }
 
##################################################
#
# cs
#
# shallow concatenate
#
# This acts like c() but makes a shallow copy, which
# is more memory efficient, but slower
# 
# Arguments:
#     
#     ... - arguments to concatenate
#
# Returns: list
#
##################################################

cs<-function(...) {
  mc<-substitute(list(...))
  if (length(mc)==0) {
    return(list())
  }
  listlength<-0
  for (i in seq(from=2, length.out=length(mc)-1)) {
      listlength<-listlength+length(eval.parent(mc[[i]]))
  }
  lastpos<-0
  retval<-vector(length=listlength,mode="list")
  for (i in seq(from=2, length.out=length(mc)-1)) {
      tmp<-eval.parent(mc[[i]])
      for (j in seq(from=1, length.out=length(tmp))) {
        retval[lastpos+j] <- tmp[j]
      }
      lastpos<-lastpos+length(tmp)
  }

  return(retval)
}

#####
# 
# mrequire
#
# Workaround CHECK complaints for require tests of _optional_ libraries
# and suppress warnings
#
#####


mrequire <-function(package, lib.loc = NULL, quietly = FALSE, warn.conflicts=TRUE,...) {
	ow<-options(warn=-1)
	retval <- require (package, lib.loc, quietly=TRUE,warn.conflicts=FALSE,...)
	options(ow)
	return(retval)
}
mrequire<-require

#####
# 
# makenames
#
# Construct names for lists
#
#####

makeNames<-function(x,prefix="") {
  if(is.null(names(x))) {
    retval<-paste(prefix,seq(1,length=length(x)),sep="")
  } else {
    retval<-names(x)
    nonames<-which(is.na(names(x))|(names(x)==""))
    retval[nonames]<-paste(prefix,nonames,sep="")
  }
  return(retval)
}

###
#
# plot a grid of plots
#
###

plotGrid<-function(plots, maxplots=100, userows=TRUE,...) {
    op <- par(no.readonly=TRUE)
    on.exit(par(op))
    if (class(plots)!="list") {
        plots<-list(plots)
    }

    plotsperwin <- min(length(plots),maxplots)
    
    nc<-nr<-ceiling(sqrt(plotsperwin))
    if (plotsperwin<=((nc-1)*nr)) { nc<-nc-1 }
    if ( (nc*nr) <  length(plots)) {
      par(ask=TRUE)
    }
    par(mar=c(0,0,0,0))
    if (userows) {
      par(mfrow=c(nr,nc))
    } else {
      par(mfcol=c(nr,nc))
    }
    sapply(plots,plot,...)
    #sapply(plots[seq(2,length=length(plots)-1)],plot,...)
    invisible()
}
  
