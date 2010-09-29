###
### Package initilization and de-initialization
###


#
# Print citation on load, stop Snow cluster, if applicable, on unload
# 

.onAttach<- function(lib, pkg) {
  # woraround spdep warning
  print(utils::citation("BARD"))
  return(TRUE)
}


.onLoad <-function(lib, pkg){
  # woraround spdep warning
 if (length(grep("^darwin",R.version$os))) {
	ow <- options(warn=-1)
  }
  retval<-mrequire("spdep")
  # workaround for maptools not detecting rgeostatus
  rgeostatus() 
 if (length(grep("^darwin",R.version$os))) {
	options(ow)
  } 
  return(retval)
}

.onUnLoad<-function(lib){
  stopBardCluster()
}

