genpops <- function(npops,truth, scenario){
  
  pops <- list(NULL)
  
  for(k in 1:npops){
    pops[[k]] <- gen1pop(K=truth$K,
                         N.1=truth$N.1,
                         p=truth$p,
                         phi=truth$phi,
                         f=truth$f,
                         sigma=truth$sigma,
                         xi=truth$xi)
  }
  
  saveRDS(pops, file= paste0("output/scenario", scenario,"/populations.RData"))

}

gen1pop <- function(K,N.1,p,phi,f,sigma,xi){
  
  ### Initialization ###
  
  D.m <- matrix(rep(NA,(K-1)*(K-1)),ncol=K-1,byrow=TRUE)
  #D.m[i,j] : number of individuals released in year i that die in year j
  
  D.u <- rep(NA,K-1)
  #D.u[j] : number of unmarked individuals that die in year j
  
  B <- rep(NA,K)
  #B[j] : number of births on year j
  
  N <- rep(NA,K)
  #N[j] : number of individuals at beggining of year j
  
  C <- rep(NA,K) #vector of length K
  #C[i] : number of individuals captured for the 1st time (marked) on year i
  
  m <- matrix(rep(NA,K*(K-1)),ncol=K,byrow=TRUE)
  #m[i,j] : number of individuals released in year i that are alive and recaptured next on year j
  
  y <- rep(NA,K)
  #y[j] : population count on year j
  
  tmp <- rep(NA,K)
  #temporary variable
  
  
  ### Year 1 distributions ###
  y[1] <- rnorm(1,N.1,sigma)
  B[1] <- rpois(1,N.1*f/2)
  C[1] <- rbinom(1,N.1+B[1],xi)
  
  
  ###Year 2 distributions###
  
  D.u[1] <- rbinom(1,N.1+B[1]-C[1],1-phi)
  D.m[1,1] <- rbinom(1,C[1],1-phi)
  N[2] <- N.1+B[1]-D.u[1]-D.m[1,1]
  y[2] <- rnorm(1,N[2],sigma)
  B[2] <- rpois(1,(N.1+B[1]-D.u[1]-D.m[1,1])*f/2)
  C[2] <- rbinom(1,N.1+B[1]+B[2]-C[1]-D.u[1],xi)
  m[1,2] <- rbinom(1,C[1]-D.m[1,1],p)
  
  ###Year 3 distributions###
  
  D.u[2] <- rbinom(1,N.1+B[1]+B[2]-C[1]-C[2]-D.u[1],1-phi)
  D.m[1,2] <- rbinom(1,C[1]-D.m[1,1]-m[1,2],1-phi)
  D.m[2,2] <- rbinom(1,C[2]+m[1,2],1-phi)
  N[3] <- N.1+B[1]+B[2]-D.u[1]-D.u[2]-D.m[1,1]-D.m[1,2]-D.m[2,2]
  y[3] <- rnorm(1,N[3],sigma)
  B[3] <- rpois(1,(N.1+B[1]+B[2]-D.u[1]-D.u[2]-D.m[1,1]-D.m[1,2]-D.m[2,2])*f/2)
  C[3] <- rbinom(1,N.1+B[1]+B[2]+B[3]-C[1]-C[2]-D.u[1]-D.u[2],xi)
  m[1,3] <- rbinom(1,C[1]-D.m[1,1]-D.m[1,2]-m[1,2],p)
  m[2,3] <- rbinom(1,C[2]-D.m[2,2]+m[1,2],p)
  
  #Years 4 to K
  for(year in 4:K){
    D.u[year-1] <- rbinom(1,N.1+sum(B[1:(year-1)])-sum(C[1:(year-1)])-sum(D.u[1:(year-2)]),1-phi)
    
    D.m[1,year-1] <- rbinom(1,C[1]-sum(D.m[1,1:(year-2)])-sum(m[1,2:(year-1)]),1-phi)
    for(t in 2:(year-2)){
      D.m[t,year-1] <- rbinom(1,C[t]+sum(m[1:(t-1),t])-sum(D.m[t,t:(year-2)])-sum(m[t,(t+1):(year-1)]),1-phi)
    }
    D.m[year-1,year-1] <- rbinom(1,C[year-1]+sum(m[1:(year-2),(year-1)]),1-phi)
    
    tmp[year] <- 0
    for(k in 1:(year-1)){
      for(l in k:(year-1)){
        tmp[year] <- tmp[year] + D.m[k,l]
      }
    }
    
    N[year] <- N.1+sum(B[1:(year-1)])-sum(D.u[1:(year-1)])-tmp[year]
    y[year] <- rnorm(1,N[year],sigma)
    B[year] <- rpois(1,(N.1+sum(B[1:(year-1)])-sum(D.u[1:(year-1)])-tmp[year])*f/2)
    C[year] <- rbinom(1,N.1+sum(B[1:year])-sum(C[1:(year-1)])-sum(D.u[1:(year-1)]),xi)
    
    m[1,year] <- rbinom(1,C[1]-sum(D.m[1,1:(year-1)])-sum(m[1,2:(year-1)]),p)
    for(t in 2:(year-2)){
      m[t,year] <- rbinom(1,C[t]+sum(m[1:(t-1),t])-sum(D.m[t,t:(year-1)])-sum(m[t,(t+1):(year-1)]),p)
    }
    m[year-1,year] <- rbinom(1,C[year-1]+sum(m[1:(year-2),year-1])-D.m[(year-1),(year-1)],p)
    
  }
  
  
  #Calculate the number of released individuals
  r<-rep(NA,K-1)
  z<-rep(NA,K-1)
  r[1] <- C[1]
  z[1] <- r[1] - sum(m[1,2:K])
  for (t in 2:(K-1)){
    r[t] <- C[t] + sum(m[1:(t-1),t])
    z[t] <- r[t] - sum(m[t,(t+1):K])
  }
  r[K] <- C[K] + sum(m[1:(K-1),K])
  
  #m.modif <- cbind(m,z)
  
  
  data <- list(m=m,y=y,C=C,r=r,z=z)#,r=r,m.modif=m.modif)
  params <- list(N=N,D.m=D.m,D.u=D.u,B=B,p=p,phi=phi,f=f,sigma=sigma,xi=xi,N.1=N.1)
  
  return(c(data,params,list("K"=K)))
}


