compare.scenarios <- function(scenarios){
  
  table1merged <- readRDS(paste0("output/scenario",scenarios[1],"/results/table1.RData"))

  for(i in scenarios[-1]){
    table1 <- readRDS(paste0("output/scenario",i,"/results/table1.RData"))
  table1merged <- mapply(rbind,table1merged,table1,SIMPLIFY=FALSE)
  }
  
  table2merged <- readRDS(paste0("output/scenario",scenarios[1],"/results/table2.RData"))
  
  for(i in scenarios[-1]){
    table2 <- readRDS(paste0("output/scenario",i,"/results/table2.RData"))
    table2merged <- mapply(rbind,table2merged,table2,SIMPLIFY=FALSE)
  }
  
  latex1 <- xtable(rbind(table1merged$phi,
                         table1merged$f,
                         table1merged$p,
                         table1merged$N.1,
                         table1merged$sigma))
  
  latex2 <- xtable(rbind(table2merged$phi,
                         table2merged$f,
                         table2merged$p,
                         table2merged$N.1,
                         table2merged$sigma))
  
 
  return(list(table1=table1merged,
              table2=table2merged,
              latex1=latex1,
              latex2=latex2))
  
}