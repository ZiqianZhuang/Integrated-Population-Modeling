# Settings & packages

if(!require(rjags)) install.packages("rjags")
if(!require(rjags)) install.packages("coda")
if(!require(rjags)) install.packages("foreach")
if(!require(rjags)) install.packages("doParallel")

setwd("/home/qianzzq/projects/def-a2belive/qianzzq/Scenario-withL")
if(!dir.exists("output")) dir.create("output")
sapply(list.files("R"), function(x) source(paste0("R/",x)))
source("R/genpops.R")

#======================================
s <- as.character(c(1,4))
npops=250
for (i in s){
  scenario<-i
  if(!dir.exists(paste0("output/scenario",scenario))) dir.create(paste0("output/scenario",scenario))
  if(!dir.exists(paste0("output/scenario",scenario,"/results"))) dir.create(paste0("output/scenario",scenario,"/results"))

# Generate all the populations -------------------------------------------
# skip this part if want to reproduce the simulations 
  if (scenario=="3"|scenario=="4") {
    K=4
  }else{
    K=8
  }
  if (scenario=="1"|scenario=="3") {
    sigma=10
  }else{
    sigma=100
  }
  N.1=500
  phi=runif(K-1,0.8,0.9)
  f=runif(K,1,4) 
  xi=runif(K,0.7,0.8)
  p=xi.true[-K]
  
  truth <- list(K=K,
                N.1=500,
                p=p,
                phi=phi,
                f=f,
                sigma=sigma,
                xi=xi)
  genpops(npops=npops,
          truth=truth,
          scenario=scenario)
  
# Analyse each population using the composite likelihood model -----------
  
pops <- readRDS(file= paste0("output/scenario",scenario,"/populations.RData"))
priorsbug.CL <- "
N.1 ~ dcat(unifpi)
for (i in 1:(K-1)){
phi[i] ~ dbeta(1,1)
p[i] ~ dbeta(1,1)
}
for(j in 1:K){
f[j]~dunif(0,10)
}
sigma ~ dunif(0,200)
"
model.CL <- genmodel.CL(priorsbug.CL) # Prepare bugs model

n.adapt.CL=10000
#n.burnin.CL=5000
n.iter.CL=20000
n.chains.CL=3
thin.CL=1
unifpi.CL <- rep(1/2000,2000)
monitor.CL <- c('p', 'N.1','f','phi','sigma')

analyse.all(pops=pops, 
            model=model.CL,
            n.adapt= n.adapt.CL,
            n.iter=n.iter.CL,
            n.chains=n.chains.CL,
            thin=thin.CL,
            unifpi=unifpi.CL,
            name="CL",
            scenario=scenario,
            monitor=monitor.CL)


# Analyse each population using the true joint likelihood model -----------

priorsbug.L <- "
N.1 ~ dcat(unifpi)
for (j in 1:K){
f[j] ~ dunif(0,10)
xi[j] ~ dbeta(1,1)
}
for (i in 1:(K-1)){
phi[i] ~ dbeta(1,1)
p[i] <-xi[i]
}
sigma ~ dunif(0,200)
"
model.L <- genmodel.L(priorsbug.L) # Prepare bugs model

n.adapt.L=10000
#n.burnin.CL=5000
n.iter.L=20000
n.chains.L=3
thin.L=1
unifpi.L <- rep(1/2000,2000)
monitor.L <- c('p', 'N.1','f','phi','xi','sigma')

analyse.all(pops=pops, 
            model=model.L,
            n.adapt= n.adapt.L,
            n.iter=n.iter.L,
            n.chains=n.chains.L,
            thin=thin.L,
            unifpi=unifpi.L,
            name="L",
            scenario=scenario,
            monitor=monitor.L)


# Summarize results -------------------------------------------------------

source("R/summarize.R")

summary.CL <- summarize(npops=npops, name="CL", scenario=scenario)
summary.L <- summarize(npops=npops, name="L", scenario=scenario)

# Compare results ---------------------------------------------------------

if(!require(rjags)) install.packages("coda")


comp <- compare(truth=truth, scenario=scenario)
capture.output(comp$table1,file=paste0("result",scenario,".txt"))
}
