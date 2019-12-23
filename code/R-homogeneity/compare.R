compare <- function(truth,
                    scenario){
  
  summary.CL <- readRDS(file= paste0("output/scenario",scenario,"/CL-summary.RData"))
  summary.L <- readRDS(file= paste0("output/scenario",scenario,"/L-summary.RData"))
  
  npops <- length(summary.CL) #must be equal
  n.chains.CL <- length(summary.CL[[1]]$heidel)
  n.chains.L <- length(summary.L[[1]]$heidel)
  
  parameters <- c("p", "phi", "sigma", "N.1", "f")
  
  for(param in parameters){
    
    #trapeze plots
    plotdata(param,
             truth[[param]],
             filename=paste0("output/scenario",scenario,"/results/plot",param,".pdf"),
             xlabel=bquote(hat(italic(~ .(as.name(param)))) ~ "via the true joint likelihood approach"),
             ylabel=bquote(hat(italic(~ .(as.name(param)))) ~ "via the composite likelihood approach"),
             summary.L=summary.L,
             summary.CL=summary.CL)
    
    #boxplots
    pdf(paste0("output/scenario",scenario,"/results/boxplot",param,".pdf"))
    par(cex=1.5,mar=c(6.5, 5.5, 2.5, 2))
    boxplot(getstats(param,"Mean",summary.L=summary.L, summary.CL=summary.CL),
            ylab=bquote(hat(italic(~ .(as.name(param)))) ~""),
            names=c("true joint \n likelihood \n approach","composite \n likelihood \n approach"),
            las=2)
    #ylim=c(0.05,0.5)) #at =c(1,2,3,4, 6,7,8,9, 11,12,13,14)
    abline(h=truth[[param]])
    graphics.off()
    
    #Histograms of the difference of the absolute error
    diffabserrorhist(param,truth[[param]],paste0("output/scenario",scenario,"/results/histerror",param,".pdf"),summary.L=summary.L, summary.CL=summary.CL)
  }
  
  table1 <- list(sigma=mcmcmeasures(npops,"sigma",truth$sigma,summary.L=summary.L, summary.CL=summary.CL, scenario=scenario)$table1,
                 phi=mcmcmeasures(npops,"phi",truth$phi,summary.L=summary.L, summary.CL=summary.CL, scenario=scenario)$table1,
                 p=mcmcmeasures(npops,"p",truth$p,summary.L=summary.L, summary.CL=summary.CL, scenario=scenario)$table1,
                 f=mcmcmeasures(npops,"f",truth$f,summary.L=summary.L, summary.CL=summary.CL, scenario=scenario)$table1,
                 N.1=mcmcmeasures(npops,"N.1",truth$N.1,summary.L=summary.L, summary.CL=summary.CL, scenario=scenario)$table1)
  
  table2 <- list(sigma=mcmcmeasures(npops,"sigma",truth$sigma,summary.L=summary.L, summary.CL=summary.CL, scenario=scenario)$table2,
                 phi= mcmcmeasures(npops,"phi",truth$phi,summary.L=summary.L, summary.CL=summary.CL, scenario=scenario)$table2,
                 p=mcmcmeasures(npops,"p",truth$p,summary.L=summary.L, summary.CL=summary.CL, scenario=scenario)$table2,
                 f=mcmcmeasures(npops,"f",truth$f,summary.L=summary.L, summary.CL=summary.CL, scenario=scenario)$table2,
                 N.1=mcmcmeasures(npops,"N.1",truth$N.1,summary.L=summary.L, summary.CL=summary.CL, scenario=scenario)$table2)
  
  saveRDS(table1, file=paste0("output/scenario",scenario,"/results/table1.RData"))
  saveRDS(table2, file=paste0("output/scenario",scenario,"/results/table2.RData"))
  
  return(list(table1= table1, table2 = table2))
}

getstats <- function(param,stat, summary.L, summary.CL){
  tmptrue <- NULL
  tmpmult <- NULL
  for(i in 1:npops){
    tmptrue <- c(tmptrue,summary.L[[i]]$summary[[1]][param,stat])
    tmpmult <- c(tmpmult,summary.CL[[i]]$summary[[1]][param,stat])
  }
  return(list(L= tmptrue,CL =tmpmult))
}




plotdata <- function(param,trueparam,filename,xlabel,ylabel,summary.L,summary.CL){
  meansparam <- getstats(param,"Mean",summary.L = summary.L,summary.CL=summary.CL)
  pdf(filename)
  par(cex=1.5,mar=c(5, 5.2, 2.5, 2))
  plotlim <- 1.02*max(abs(c(meansparam[[1]],meansparam[[2]])-trueparam))
  plot(NA,ylim=c(max(0,trueparam-plotlim),trueparam+plotlim)
       ,xlim=c(max(0,trueparam-plotlim),trueparam+plotlim),xlab=xlabel, ylab=ylabel)
  polygon(x=c(0,trueparam,2*trueparam,4*trueparam,0),y=c(2*trueparam,trueparam,2*trueparam,4*trueparam,4*trueparam),col=gray(0.9),border=NA)		
  polygon(x=c(0,trueparam,2*trueparam),y=c(0,trueparam,0),col=gray(0.9),border=NA)
  points(meansparam[[1]],meansparam[[2]])
  graphics.off()
  
}

diffabserrorhist <- function(paramname,paramtrue,filename,summary.L, summary.CL){
  pdf(filename)
  par(cex=1.5,mar=c(5, 5.5, 2.5, 2))
  hist(abs(getstats(paramname,"Mean",summary.L=summary.L, summary.CL=summary.CL)[[1]]-paramtrue)-abs(getstats(paramname,"Mean",summary.L=summary.L, summary.CL=summary.CL)[[2]]-paramtrue),
       breaks=15,
       freq=TRUE,
       main=NULL,
       xlab=expression(AE[L] - AE[L^c]),
       ylab="Frequency")
  graphics.off()
}

getinterval <- function(popid,param,lowup,summaryobj){
  return(summaryobj[[popid]]$hpd[[1]][param,lowup]) #only using the first chain for now
}

mcmcmeasures <- function(npops,param,paramtrue,summary.L, summary.CL, scenario){
  
  parammean <- getstats(param,"Mean", summary.L=summary.L,summary.CL=summary.CL )
  bias <- (colMeans((simplify2array(parammean)))-paramtrue)
  relbias <- bias/paramtrue
  mse <- colMeans((simplify2array(parammean)-paramtrue)^2)
  rmse <- sqrt(mse)
  rrmse <- sqrt(mse)/paramtrue
  tmp <- abs(simplify2array(parammean)-paramtrue)
  biasratio <- bias/sqrt(mse-bias^2)
  paramSD <- getstats(param,"SD", summary.L=summary.L,summary.CL=summary.CL)
  meanSD <- (colMeans((simplify2array(paramSD))))
  
  lowerboundtrue <- 	unlist(lapply(1:npops,function(i) getinterval(i,param,"lower", summary.L)))
  upperboundtrue <- 	unlist(lapply(1:npops,function(i) getinterval(i,param,"upper", summary.L)))
  lowerboundmult <- 	unlist(lapply(1:npops,function(i) getinterval(i,param,"lower", summary.CL)))
  upperboundmult <- 	unlist(lapply(1:npops,function(i) getinterval(i,param,"upper", summary.CL)))
  
  meanlength <- c(mean(upperboundtrue-lowerboundtrue), mean(upperboundmult-lowerboundmult))
  rellength <- meanlength/paramtrue
  
  coverageproba <- c(mean(upperboundtrue>=paramtrue & lowerboundtrue<=paramtrue), mean(upperboundmult>=paramtrue & lowerboundmult<=paramtrue))
  
  closeness <- mean(tmp[,1]<=tmp[,2])
  SDsmallest <- mean(paramSD[[1]]<=paramSD[[2]])
  CIsmallest <- mean(c(upperboundtrue-lowerboundtrue <= upperboundmult-lowerboundmult))
  
  table1 <- cbind(rep(scenario,2),c("$L$","$L^c$"),as.data.frame(cbind(paramtrue,bias,rmse,biasratio,meanSD,meanlength,coverageproba)))
  table2 <- cbind(scenario,as.data.frame(t(c(closeness,SDsmallest,CIsmallest))))
  
  names(table1) <- c("Scenario","Method","True value","Bias", "RMSE", "BR", "SD","LCI","CP")
  names(table2) <- c("Scenario", "AE", "SD", "LCI") #absolute error
  
  return(list(table1=table1,table2=table2))
}


