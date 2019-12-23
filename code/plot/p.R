###p
##packages
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(Rmisc)) install.packages("Rmisc")
if(!require(plyr)) install.packages("plyr")
if(!require(tibble)) install.packages("tibble")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(xlsx)) install.packages("xlsx")

#settings
theme_set(theme_classic())

#Scenario 1
data<-read.xlsx("p.xlsx",sheetName="S1",header=T)
data<-na.omit(data)
data$estimate<-data$Truevalue+data$Bias
data$LCI<-data$LCI/2
data$estimate[which(data$Converge=="no")]<-NA
data$RMSE[which(data$Converge=="no")]<-NA
data$CP[which(data$Converge=="no")]<-NA
data$LCI[which(data$Converge=="no")]<-0
data$Year<-as.factor(data$Year)


f1p<-ggplot(data, aes(Year, estimate)) +
  geom_pointrange(
    aes(ymin = estimate-LCI, ymax = estimate+LCI, color = Method,shape=Method),
    position = position_dodge(0.8)
  )+coord_cartesian(ylim = c(0.65,0.85))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))

f1.p=f1p+
  geom_errorbar(aes(width=0.8,ymax=Truevalue,ymin=Truevalue),color="black",linetype="dashed")+
  guides(color=F,shape=F)+
  ylab("estimate of p")

rmse.plot1<-ggplot(data=data,mapping= aes(x=Year,y=RMSE,colour=Method,shape=Method))+
  coord_cartesian(ylim = c(0, 0.02))+
  geom_point(position=position_dodge(width=0.4))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))+
  ylab("RMSE of p")+guides(color=F,shape=F)

cp.plot1<-ggplot(data=data,mapping= aes(x=Year,y=CP,colour=Method,shape=Method))+
  geom_point(position=position_dodge(width=0.4))+
  coord_cartesian(ylim = c(0.9,1))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))+
  guides(color=F,shape=F)+ ylab("CP of p")+
  geom_hline(aes(yintercept=0.95), linetype="dashed", color="black", size=0.5)

s1<-grid.arrange(f1.p,rmse.plot1,cp.plot1,nrow=3)

#Scenario2
data<-read.xlsx("p.xlsx",sheetName="S2",header=T)
data<-na.omit(data)
data$estimate<-data$Truevalue+data$Bias
data$LCI<-data$LCI/2
data$estimate[which(data$Converge=="no")]<-NA
data$RMSE[which(data$Converge=="no")]<-NA
data$CP[which(data$Converge=="no")]<-NA
data$LCI[which(data$Converge=="no")]<-0
data$Year<-as.factor(data$Year)

f2p<-ggplot(data, aes(Year, estimate)) +
  geom_pointrange(
    aes(ymin = estimate-LCI, ymax = estimate+LCI, color = Method,shape=Method),
    position = position_dodge(0.8)
  )+coord_cartesian(ylim = c(0.65,0.85))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))

f2.p=f2p+
  geom_errorbar(aes(width=0.8,ymax=Truevalue,ymin=Truevalue),color="black",linetype="dashed")+
  guides(color=F,shape=F)+
  ylab("")

rmse.plot2<-ggplot(data=data,mapping= aes(x=Year,y=RMSE,colour=Method,shape=Method))+
  coord_cartesian(ylim = c(0, 0.02))+
  geom_point(position=position_dodge(width=0.4))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))+
  ylab("")+guides(color=F,shape=F)

cp.plot2<-ggplot(data=data,mapping= aes(x=Year,y=CP,colour=Method,shape=Method))+
  geom_point(position=position_dodge(width=0.4))+
  coord_cartesian(ylim = c(0.9,1))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))+
  guides(color=F,shape=F)+ ylab("")+
  geom_hline(aes(yintercept=0.95), linetype="dashed", color="black", size=0.5)

s2<-grid.arrange(f2.p,rmse.plot2,cp.plot2,nrow=3)

#Scenario 3
data<-read.xlsx("p.xlsx",sheetName="S3",header=T)
data<-na.omit(data)
data$estimate<-data$Truevalue+data$Bias
data$LCI<-data$LCI/2
data$estimate[which(data$Converge=="no")]<-NA
data$RMSE[which(data$Converge=="no")]<-NA
data$CP[which(data$Converge=="no")]<-NA
data$LCI[which(data$Converge=="no")]<-0
data$Year<-as.factor(data$Year)

f3p<-ggplot(data, aes(Year, estimate)) +
  geom_pointrange(
    aes(ymin = estimate-LCI, ymax = estimate+LCI, color = Method,shape=Method),
    position = position_dodge(0.4)
  )+coord_cartesian(ylim = c(0.65,0.85))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))

f3.p=f3p+
  geom_errorbar(aes(width=0.5,ymax=Truevalue,ymin=Truevalue),color="black",linetype="dashed")+
  guides(color=F,shape=F)+ylab("")

rmse.plot3<-ggplot(data=data,mapping= aes(x=Year,y=RMSE,colour=Method,shape=Method))+
  coord_cartesian(ylim = c(0.0, 0.02))+
  geom_point(position=position_dodge(width=0.4))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))+
  ylab("")+guides(color=F,shape=F)

cp.plot3<-ggplot(data=data,mapping= aes(x=Year,y=CP,colour=Method,shape=Method))+
  geom_point(position=position_dodge(width=0.4))+
  coord_cartesian(ylim = c(0.9,1))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))+
  guides(color=F,shape=F)+ ylab("")+
  geom_hline(aes(yintercept=0.95), linetype="dashed", color="black", size=0.5)

s3<-grid.arrange(f3.p,rmse.plot3,cp.plot3,nrow=3)

#Scenario 4
data<-read.xlsx("p.xlsx",sheetName="S4",header=T)
data<-na.omit(data)
data$estimate<-data$Truevalue+data$Bias
data$LCI<-data$LCI/2
data$estimate[which(data$Converge=="no")]<-NA
data$RMSE[which(data$Converge=="no")]<-NA
data$CP[which(data$Converge=="no")]<-NA
data$LCI[which(data$Converge=="no")]<-0
data$Year<-as.factor(data$Year)

f4p<-ggplot(data, aes(Year, estimate)) +
  geom_pointrange(
    aes(ymin = estimate-LCI, ymax = estimate+LCI, color = Method,shape=Method),
    position = position_dodge(0.4)
  )+coord_cartesian(ylim = c(0.65,0.85))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))

f4.p=f4p+
  geom_errorbar(aes(width=0.5,ymax=Truevalue,ymin=Truevalue),color="black",linetype="dashed")+
  guides(color=F,shape=F)+ylab("")

rmse.plot4<-ggplot(data=data,mapping= aes(x=Year,y=RMSE,colour=Method,shape=Method))+
  geom_point(position=position_dodge(width=0.4))+
  coord_cartesian(ylim = c(0, 0.02))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))+
  ylab("")+guides(color=F,shape=F)

cp.plot4<-ggplot(data=data,mapping= aes(x=Year,y=CP,colour=Method,shape=Method))+
  geom_point(position=position_dodge(width=0.4))+
  coord_cartesian(ylim = c(0.9,1))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))+
  guides(color=F,shape=F)+ ylab("")+
  geom_hline(aes(yintercept=0.95), linetype="dashed", color="black", size=0.5)

s4<-grid.arrange(f4.p,rmse.plot4,cp.plot4,nrow=3)

#Scenario 5-8
data<-read.xlsx("p.xlsx",sheetName="homo",header=T)
data<-na.omit(data)
data$estimate<-data$Truevalue+data$Bias
data$LCI<-data$LCI/2
data$estimate[which(data$Converge=="no")]<-NA
data$RMSE[which(data$Converge=="no")]<-NA
data$CP[which(data$Converge=="no")]<-NA
data$LCI[which(data$Converge=="no")]<-0
data$Scenario<-as.factor(data$Scenario)

f5p<-ggplot(data, aes(Scenario, estimate)) +
  geom_pointrange(
    aes(ymin = estimate-LCI, ymax = estimate+LCI, color = Method,shape=Method),
    position = position_dodge(0.5)
  )+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))

f5.p=f5p+
  geom_hline(aes(yintercept=Truevalue), linetype="dashed", color="black", size=0.5)+
  guides(color=F,shape=F)+
  ylab("")

rmse.plot5<-ggplot(data=data,mapping= aes(x=Scenario,y=RMSE,colour=Method,shape=Method))+
  geom_point(position=position_dodge(width=0.4))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))+
  coord_cartesian(ylim = c(0, 0.01))+
  ylab("")+guides(color=F,shape=F)

cp.plot5<-ggplot(data=data,mapping= aes(x=Scenario,y=CP,colour=Method,shape=Method))+
  geom_point(position=position_dodge(width=0.4))+
  coord_cartesian(ylim = c(0.9,1))+
  scale_color_manual(values = c("#87ceeb", "#ff0000"))+
  guides(color=F,shape=F)+ ylab("")+
  geom_hline(aes(yintercept=0.95), linetype="dashed", color="black", size=0.5)

s5<-grid.arrange(f5.p,rmse.plot5,cp.plot5,nrow=3)

##Multiplot
figure.p<-grid.arrange(s1,s2,s3,s4,s5,nrow=1)

##save as PNG
ggsave("Graph of p.png", plot = figure.p, device = "png",
       scale = 1, width = 390, height = 220, units ="mm",
       dpi = 300)
