#########################################################################
#
# bard.basics - Despite the name, this code serves as the framework
#                   for getting BARD started, setting BARD's runtime 
#                   features, and plotting the results of Simulated
#                   Annealing.  There are 4 main areas of interest to get
#                   get BARD running and to set BARD's runtim parameters.
#                   First, we must import a map with or without a GAL file.
#                   Second, we set the number of districts and ask one of 
#                   the many initialization functions to give us a starting
#                   seed for optimization.  Third, we tell the optim or 
#                   simulated annealing function which cost functions to
#                   use (population equality, compactness, contiguity, etc.),
#                   what weight to give to these costs, and which plan
#                   generation function to utilize when drawing blocks to 
#                   make trades.  Finally, we plot the final results of 
#                   simmulated annealing and the demographic statistics
#                   of the final "optimized" map.
#
#########################################################################

###############################################################
#
# Step 1: Read in the shapefile.  The various functions
#         available to read in a shapefile are found in the
#         R\bardUtil file.  Below we are using the Wisconsin
#         Ward map and we are passing in a Queen-defined
#         contiguity GAL file.
#
###############################################################

 library(BARD)
 
 # read in a shapefile with demographic data
  suffolk.map <- importBardShape(
      file.path(system.file("shapefiles", package="BARD"),"suffolk_tracts")
  )
    

################################################################
#
# Step 2: Set the number of districts (ndists) and create an 
#         initial map seed for simulated annealing.  Below we
#         are currently using k-means as the initial seed and
#         we must plot the results of k-means, after it has 
#         generated, if we want to see the initial seed map.  
#         However, some initialization function do this plotting
#         internally (i.e. the greedy methods).  Other
#         initialization functions can be found in the R\bardPlan
#         file.
#
################################################################

  # choose number of districts
  numberdists <- 5
  kplan <- createKmeansPlan(suffolk.map,numberdists)
  rplan <- createRandomPlan(suffolk.map,numberdists)
  rplan2 <- createRandomPopPlan(suffolk.map,numberdists)
  plot(kplan)
                                   
  # edit interactively
   rplan<-editPlanInteractive(rplan,calcPopScore,predvar="POP")


#####################################################################
#
# Step 3: Telling plan refinement annaling which cost functions to 
#         use, what weight to give these costs, and which plan
#         generation function to utilize when drawing candidate blocks
#         for trading from the current state of the districts during
#         optimization.
#
#####################################################################

  myCombinedS<-function(plan,lastscore=NULL,...) {
  
    plast<-NULL
    clast<-NULL
    ctlast<-NULL
    if (!is.null(lastscore)) {
      plast<-attr(lastscore,"plast")
      clast<-attr(lastscore,"clast")
      ctlast<-attr(lastscore,"ctlast")
    }
    
    pscore<-calcPopScore(plan,lastscore=plast,...)
    cscore<-calcLWCompactScore(plan,lastscore=clast,...)
    ctscore<-calcContiguityScore(plan,lastscore=ctlast,...)
    
    combined <- 
        exp(cscore)+10*exp(pscore)+100*exp(ctscore)
    attr(combined,"plast")<-pscore
    attr(combined,"clast")<-cscore
    attr(combined,"ctlast")<-ctscore
    return(combined)
  }
  
  
    
  
  myScore<-function(plan,...)  {
    return(calcContiguityScore(plan,...))
  }                                                                    
  

########################################################################
#
# Step 4: Run plan refinement, compare the solution and scores 
#
########################################################################   

 reportPlans(plans=list("kmeans"=kplan,"random 1"=rplan,"random pop"=rplan2), doplot=TRUE)

# Nelder is ineffective -- just a demo
improvedRplan<-refineNelderPlan(plan=rplan2, score.fun=myScore, displaycount=100, historysize=0, dynamicscoring=FALSE,  tracelevel=1, maxit=100)

# This is for real -- really slow though
#improvedRplan<-refineAnnealPlan(plan=rplan2, score.fun=myCombinedS, displaycount=100, historysize=0, dynamicscoring=FALSE,  tracelevel=1)


########################################################################
#
# Step 5: Plan sampling and profiling 
#
########################################################################   

samples<-samplePlans(kplan, score.fun=myScore, ngenplans=20, gen.fun = "createRandomPlan", refine.fun="refineNelderPlan",refine.args=list(maxit=200,dynamicscoring=TRUE))

profplans<-profilePlans(  list(kplan,rplan), score.fun=calcContiguityScore, addscore.fun=calcPopScore, numevals=2, weight=c(0,.5,1), refine.fun="refineNelderPlan",refine.args=list(maxit=200,dynamicscoring=TRUE) )
 
summary(samples)
plot(summary(samples))
reportPlans(samples)
plot(summary(profplans))
