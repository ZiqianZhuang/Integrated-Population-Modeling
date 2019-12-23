###N
##packages

if(!require(dplyr)) install.packages("dplyr")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(Rmisc)) install.packages("Rmisc")
if(!require(plyr)) install.packages("plyr")
if(!require(tibble)) install.packages("tibble")
if(!require(gridExtra)) install.packages("gridExtra")


##read the table
data<-read.csv("N.1.csv",header=T)
data<-na.omit(data)
data$estimate<-data$Truevalue+data$Bias
data$length<-data$LCI/2
data$estimate[which(data$Converge=="no")]<-NA
data$RMSE[which(data$Converge=="no")]<-NA
data$CP[which(data$Converge=="no")]<-NA
data$length[which(data$Converge=="no")]<-0
data$Scenario<-as.factor(data$Scenario)

#generate the graph of bias
f.N<-ggplot(data,aes(Scenario,estimate))+
  geom_pointrange(
    aes(ymin=estimate-length,ymax=estimate+length,color=Method,shape=Method),
    position=position_dodge(0.8)
  )+
  scale_color_manual(values=c("#87ceeb", "#ff0000"))

f.N=f.N+
  geom_hline(aes(yintercept=Truevalue), linetype="dashed", color="black", size=0.5)+
  guides(color=F,shape=F)+xlab("scenario")+ylab("estiamte of N.1")

#generate the graph of RMSE
rmse.plot.N<-ggplot(data=data,mapping= aes(x=Scenario,y=RMSE,colour=Method,shape=Method))+
  geom_point(position=position_dodge(width=0.4))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))+
  ylab("RMSE of N.1")+xlab("scenario")+
  guides(color=F,shape=F)

#generate the graph of CP
cp.plot.N<-ggplot(data=data,mapping= aes(x=Scenario,y=CP,colour=Method,shape=Method))+
  geom_point(position=position_dodge(width=0.4))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))+
  ylab("CP of N.1")+xlab("scenario")+guides(color=F,shape=F)+
  geom_hline(aes(yintercept=0.95), linetype="dashed", color="black", size=0.5)

#Multiplot
figure.N<-grid.arrange(f.N,rmse.plot.N,cp.plot.N,nrow=1)

##save as PNG
ggsave("Graph of N.1.png", plot = figure.N, device = "png",
       scale = 1, width = 380, height = 108, units ="mm",
       dpi = 300)

