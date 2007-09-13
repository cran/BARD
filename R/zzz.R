###
### Package initilization and de-initialization
###


#
# Print citation on load, stop Snoe cluster, if applicable, on unload
# 

.onLoad <- function(lib, pkg) {
  print(utils::citation("BARD"))
  return(TRUE)
}

.onUnLoad<- function (lib) {
  stopBardCluster()
}

