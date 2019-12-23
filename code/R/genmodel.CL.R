genmodel.CL <- function(priorsbug){
  
  
  model <- paste0("
      model
      { \n\n", 
      priorsbug,
      "\n\n
      #xi <- p
      
      N[1] <- 0
      for(i in 1:(K-1)){
       probas[i,i+1]<-phi[i]*p[i]
      }
      for(i in 1:(K-2)){
      for(j in (i+2):K){
      probas[i,j]<-prod(phi[i:(j-1)])*p[j-1]*prod(1-p[i:(j-2)])
      }
      }
      
      for(i in 1:(K-1)){
      probas[i,K+1] <- 1-sum(probas[i,(i+1):K])
      }
      
      for(i in 1:(K-1)){
      m[i,(i+1):(K+1)] ~ dmulti(probas[i,(i+1):(K+1)],r[i])
      }
      
      y[1] ~ dnorm(N.1,1/sigma/sigma)
      y[2] ~ dnorm(N[2],1/sigma/sigma)
      B[1] ~ dpois(N.1*f[1]/2)
      N[2] ~ dbin(phi[1],N.1+B[1])
      
      for(i in 3:K){
      y[i] ~ dnorm(N[i],1/sigma/sigma)
      B[i-1] ~ dpois(N[i-1]*f[i-1]/2)
      N[i] ~ dbin(phi[i-1],N[i-1]+B[i-1])
      }
      
      B[K] ~ dpois(N[K]*f[K]/2)
      
      
      }")
  
  cat(model, file="modelCL.bug")

  
  return(model)
  
}