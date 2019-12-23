analyse.all <- function(model, pops, unifpi, n.iter, n.chains=3, n.adapt, thin=1, monitor, name, scenario){
  
  npops <- length(pops)
    
  library(foreach) ##Use parallel computing to speed up the analyses
  library(doParallel)
  cores=detectCores()
  cl <- makeCluster(cores[1]-1)
  clusterSetRNGStream(cl)
  registerDoParallel(cl)
  timer <- proc.time()
  check <- foreach(i=1:npops, .packages = c("rjags", "coda"), .export = c("analyse",
                                                                  "trace")) %dopar% {
    tmp <- analyse(pop=pops[[i]], 
                   model=model,
                   n.adapt= n.adapt,
                   n.iter=n.iter,
                   n.chains=n.chains,
                   thin=thin,
                   unifpi=unifpi,
                   pop.id=i,
                   name=name,
                   scenario=scenario,
                   monitor=monitor)
    trace(tmp, pop.id=i, name=name, scenario=scenario)
    
    return(i)
  }
  proc.time() - timer
  stopCluster(cl)
  
  return(check)
}


analyse <- function(model, pop, unifpi, n.iter, n.chains, n.adapt, thin, monitor, pop.id, name, scenario){
   if(name=="CL") inits <- list(p=pop$p,phi=pop$phi,'sigma'=pop$sigma,f=pop$f,'N.1'=pop$N.1,N=pop$N,B=pop$B)
    if(name=="L") inits <- list(p=pop$p,phi=pop$phi,'sigma'=pop$sigma,'xi'=pop$xi,f=pop$f,'N.1'=pop$N.1,B=pop$B,D.m=pop$D.m,D.u=pop$D.u)
   
   if(name=="CL") data <- list('K' = pop$K, 'y'=pop$y, 'm'=cbind(pop$m,pop$z), 'r'=pop$r, 'unifpi'=unifpi)
   if(name=="L")  data <- list('K' = pop$K, 'y'=pop$y, 'm'=pop$m, 'C'=pop$C, 'unifpi'=unifpi)
   
   jags <- jags.model(textConnection(model), data = data, inits, n.chains = n.chains, n.adapt = n.adapt)
   
   mcmc <- coda.samples(jags, variable.names=monitor, n.iter = n.iter, thin=thin)
   #mcmc<-window(mcmc,start=5001,end=30000)  
   saveRDS(mcmc, file=paste0("output/scenario", scenario, "/", name, "-", pop.id, ".RData"))
   
   return(mcmc)
   
}
