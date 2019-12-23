genmodel.L <- function(priorsbug){
  
  
  model <- paste0("
      model
      { \n\n", 
      priorsbug,
      "\n\n
      ### Year 1 distributions ###
      y[1] ~ dnorm(N.1,1/sigma/sigma)
      B[1] ~ dpois(N.1*f[1]/2)
      C[1] ~ dbin(xi[1],N.1+B[1])
      
      
      ###Year 2 distributions###
      
      D.u[1] ~ dbin(1-phi[1],N.1+B[1]-C[1])
      D.m[1,1] ~ dbin(1-phi[1],C[1])
      y[2] ~ dnorm(N.1+B[1]-D.u[1]-D.m[1,1],1/sigma/sigma)
      B[2] ~ dpois((N.1+B[1]-D.u[1]-D.m[1,1])*f[2]/2)
      C[2] ~ dbin(xi[2],N.1+B[1]+B[2]-C[1]-D.u[1])
      m[1,2] ~ dbin(p[1],C[1]-D.m[1,1])
      
      ###Year 3 distributions###
      
      D.u[2] ~ dbin(1-phi[2],N.1+B[1]+B[2]-C[1]-C[2]-D.u[1])
      D.m[1,2] ~ dbin(1-phi[2],C[1]-D.m[1,1]-m[1,2])
      D.m[2,2] ~ dbin(1-phi[2],C[2]+m[1,2])
      y[3] ~ dnorm(N.1+B[1]+B[2]-D.u[1]-D.u[2]-D.m[1,1]-D.m[1,2]-D.m[2,2],1/sigma/sigma)
      B[3] ~ dpois((N.1+B[1]+B[2]-D.u[1]-D.u[2]-D.m[1,1]-D.m[1,2]-D.m[2,2])*f[3]/2)
      C[3] ~ dbin(xi[3],N.1+B[1]+B[2]+B[3]-C[1]-C[2]-D.u[1]-D.u[2])
      m[1,3] ~ dbin(p[2],C[1]-D.m[1,1]-D.m[1,2]-m[1,2])
      m[2,3] ~ dbin(p[2],C[2]-D.m[2,2]+m[1,2])
      
      #Years 4 to K
      for(year in 4:K){
      D.u[year-1] ~ dbin(1-phi[year-1],N.1+sum(B[1:(year-1)])-sum(C[1:(year-1)])-sum(D.u[1:(year-2)]))
      
      D.m[1,year-1] ~ dbin(1-phi[year-1],C[1]-sum(D.m[1,1:(year-2)])-sum(m[1,2:(year-1)]))
      for(t in 2:(year-2)){
      D.m[t,year-1] ~ dbin(1-phi[year-1],C[t]+sum(m[1:(t-1),t])-sum(D.m[t,t:(year-2)])-sum(m[t,(t+1):(year-1)]))
      }
      D.m[year-1,year-1] ~ dbin(1-phi[year-1],C[year-1]+sum(m[1:(year-2),(year-1)]))
      
      for(k in 1:(year-1)){
      tmpk[year,k] <- sum(D.m[k,k:(year-1)])
      }
      tmp[year] <- sum(tmpk[year,1:(year-1)])
      
      y[year] ~ dnorm(N.1+sum(B[1:(year-1)])-sum(D.u[1:(year-1)])-tmp[year],1/sigma/sigma)
      B[year] ~ dpois((N.1+sum(B[1:(year-1)])-sum(D.u[1:(year-1)])-tmp[year])*f[year]/2)
      C[year] ~ dbin(xi[year],N.1+sum(B[1:year])-sum(C[1:(year-1)])-sum(D.u[1:(year-1)]))
      
      m[1,year] ~ dbin(p[year-1],C[1]-sum(D.m[1,1:(year-1)])-sum(m[1,2:(year-1)]))
      for(t in 2:(year-2)){
      m[t,year] ~ dbin(p[year-1],C[t]+sum(m[1:(t-1),t])-sum(D.m[t,t:(year-1)])-sum(m[t,(t+1):(year-1)]))
      }
      m[year-1,year] ~ dbin(p[year-1],C[year-1]+sum(m[1:(year-2),year-1])-D.m[(year-1),(year-1)])
      
      }
}")
  
  cat(model, file="modelL.bug")

  
  return(model)
  
}