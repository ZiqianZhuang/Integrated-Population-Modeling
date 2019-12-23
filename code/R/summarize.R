summarize <- function(npops, name, scenario){
  
  #summary<-list(NULL)
  #hpd<-list(NULL)
  #heidel<-list(NULL)
  
  library(foreach)
  library(doParallel)
  cores=detectCores()
  cl <- makeCluster(cores[1]-1)
  clusterSetRNGStream(cl)
  registerDoParallel(cl)
  
  finalMatrix <- foreach(i=1:npops, .packages = c("coda")) %dopar% {
  
  #for(i in 1:npops){
    mcmc <- readRDS(file=paste0("output/scenario",scenario,"/",name,"-",i,".RData"))
            #summary[[i]]<-summary(mcmc)
                      #  hpd[[i]]<-HPDinterval(mcmc)
                      #  heidel[[i]]<-heidel.diag(mcmc)
    summary<-summary(mcmc)
    hpd<-HPDinterval(mcmc)
    heidel<-heidel.diag(mcmc)
  
  
  return(list(summary=summary, hpd=hpd, heidel=heidel))
}
stopCluster(cl)

saveRDS(finalMatrix, file= paste0("output/scenario",scenario,"/",name,"-summary.RData"))
  
  #saveRDS(summary, file= paste0("output/",name,"-summary.RData"))
  #saveRDS(hpd, file= paste0("output/",name,"-hpd.RData"))
  #saveRDS(heidel, file= paste0("output/",name,"-heidel.RData"))
  
  return(finalMatrix)
}
