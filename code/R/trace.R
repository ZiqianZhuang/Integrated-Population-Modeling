trace <- function(mcmc, name, pop.id,scenario){
  
  dir <- paste0("output/scenario",scenario,"/trace")
  
  if(!dir.exists(dir)) dir.create(dir)
  png(paste0(dir,"/",name,pop.id,"-plot%02d.png"))
  plot(mcmc)
  dev.off()
  
}