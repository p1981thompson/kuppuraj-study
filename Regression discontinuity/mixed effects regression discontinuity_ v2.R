#pilot data analysis and tentative analysis script for the learning task
setwd("C:/Users/pthompson/Dropbox/SL and WM paper/Revision/Pilot_Data_Revised task/Version_2/more_participants")
#required packages

install.packages("optimx")
library(optimx)
library(lme4)
library(ggplot2)
library(doBy)

namelist=c('002','003','004','005','006','007')#,'008','009' )
nsubs=length(namelist)

main.data<-data.frame(ID=factor(), SetInd=integer(), Type=integer(), TargetRT=integer())

for (i in 1:nsubs){
  
  myname=namelist[i]
  mycsv=paste(myname,".csv",sep="")
  mydata= read.csv(mycsv)  # read csv file 
  #get rid of RTs of inaacurate responses and any RT greater than 3000 ms:replace with NA. 
  Rwdata=mydata
  rawdata=Rwdata[ ,c(1,10,12,26)]
  #in the original analysis the following section will be removed 
  
  #######this bit ensures inaccurate becomes, so not eliminated in next section, and at least RT can be extracted. 
  rawdata$TargetRT[rawdata$Type==19 & rawdata$TargetRT>3000]<-2999
  ##############
  rawdata$TargetRT[rawdata$TargetACC==0]<-NA
  rawdata$TargetRT[rawdata$TargetRT<100]<-NA
  rawdata$TargetRT[rawdata$TargetRT>3000]<-NA
  
  RWdata<-rawdata
  
  #rename the types so median can be taken for each block 
  RWdata$Type[RWdata$Type==1]<- "Adj_D"
  RWdata$Type[RWdata$Type==2]<- "Adj_D"
  RWdata$Type[RWdata$Type==3]<- "Adj_D"
  RWdata$Type[RWdata$Type==4]<- "Adj_P"
  RWdata$Type[RWdata$Type==5]<- "Adj_P"
  RWdata$Type[RWdata$Type==6]<- "Adj_P"
  RWdata$Type[RWdata$Type==7]<- "Adj_P"
  RWdata$Type[RWdata$Type==8]<- "Adj_P"
  RWdata$Type[RWdata$Type==9]<- "Adj_P"
  RWdata$Type[RWdata$Type==10]<- "Non_D"
  RWdata$Type[RWdata$Type==11]<- "Non_D"
  RWdata$Type[RWdata$Type==12]<- "Non_D"
  RWdata$Type[RWdata$Type==13]<- "Non_P"
  RWdata$Type[RWdata$Type==14]<- "Non_P"
  RWdata$Type[RWdata$Type==15]<- "Non_P"
  RWdata$Type[RWdata$Type==16]<- "Non_P"
  RWdata$Type[RWdata$Type==17]<- "Non_P"
  RWdata$Type[RWdata$Type==18]<- "Non_P"
  RWdata$Type[RWdata$Type==19]<- "rand"
  
  RWdata$ID<-substring(RWdata$ID,7,7)
  RWdata$Type<-as.factor(RWdata$Type)
  RWdata$Type<-factor(RWdata$Type,levels=c("rand", "Adj_D", "Adj_P", "Non_D", "Non_P"))
  
  detaildata<- summaryBy(TargetRT ~ SetInd+Type,  data=RWdata,
                         FUN=c(median), na.rm=TRUE)
  
  detaildata$ID<-rep(RWdata$ID[4],length(detaildata[,1]))
  
  names(detaildata)<-c("SetInd", "Type", "TargetRT", "ID")
  
  main.data<-rbind(main.data,detaildata)
  
}



main.data$ID <- as.factor(main.data$ID)
main.data$log_TargetRT<-log(main.data$TargetRT)
  
################################################################################################################################################
  
main.data2<-main.data  

main.data2$broke1<-ifelse(main.data2$SetInd %in% c(1:24),1,0) 
main.data2$broke2<-ifelse(main.data2$SetInd %in% c(33:40),1,0) 

bp1=0.3896708 #cutpoint 1
bp2=1.082419 #cutpoint 2
#
b1 <- function(x, bp1) ifelse(x < bp1, bp1 - x, 0)
b2 <- function(x, bp1, bp2) ifelse(x >= bp1 & x < bp2, x - bp1, 0)
b3 <- function(x, bp2) ifelse(x < bp2, 0, x - bp2)

main.data2$SetInd_c<-scale(main.data2$SetInd)

mod1d2 <- lmer(log_TargetRT ~ Type + b1(SetInd_c, bp1) + b2(SetInd_c, bp1,bp2) + b3(SetInd_c,bp2) + Type*b1(SetInd_c, bp1) + Type*b2(SetInd_c, bp1,bp2) + Type*b3(SetInd_c, bp2) +(broke1+broke2+b1(SetInd_c, bp1) + b2(SetInd_c, bp1,bp2) + b3(SetInd_c, bp2)| ID), data = main.data2, REML = TRUE)

newdat1d<-expand.grid(Type=unique(main.data2$Type),SetInd_c=unique(main.data2$SetInd_c)[1:24],ID=unique(main.data2$ID))
newdat2d<-expand.grid(Type=unique(main.data2$Type),SetInd_c=unique(main.data2$SetInd_c)[25:32],ID=unique(main.data2$ID))
newdat3d<-expand.grid(Type=unique(main.data2$Type),SetInd_c=unique(main.data2$SetInd_c)[33:40],ID=unique(main.data2$ID))

newdat1d$broke1<-ifelse(newdat1d$SetInd %in% unique(main.data2$SetInd_c)[1:24],1,0) 
newdat1d$broke2<-ifelse(newdat1d$SetInd %in% unique(main.data2$SetInd_c)[33:40],1,0) 

newdat2d$broke1<-ifelse(newdat2d$SetInd %in% unique(main.data2$SetInd_c)[1:24],1,0) 
newdat2d$broke2<-ifelse(newdat2d$SetInd %in% unique(main.data2$SetInd_c)[33:40],1,0) 

newdat3d$broke1<-ifelse(newdat3d$SetInd %in% unique(main.data2$SetInd_c)[1:24],1,0) 
newdat3d$broke2<-ifelse(newdat3d$SetInd %in% unique(main.data2$SetInd_c)[33:40],1,0) 



  library(RColorBrewer)
  library(ggplot2)

  ggplot(main.data2, aes(x = SetInd_c, y = log_TargetRT,color=Type)) + 
  geom_point(alpha=0.35) + 
  geom_vline(aes(xintercept = 0.3896708), color = 'grey', size = 1, linetype = 'dashed') + 
  geom_vline(aes(xintercept = 1.082419), color = 'grey', size = 1, linetype = 'dashed') +
  geom_line(data=newdat1d,aes(y=predict(mod1d2,newdata=newdat1d)),size = .75)+
    geom_line(data=newdat2d,aes(y=predict(mod1d2,newdata=newdat2d)),size = .75)+
    geom_line(data=newdat3d,aes(y=predict(mod1d2,newdata=newdat3d)),size = .75)+
  theme_bw()+facet_grid(~ID)+ scale_fill_brewer(palette="Set1")+
  theme(legend.position = "top",strip.text=element_text(size=12),axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

##########################################################################################
  
  
  #regression diagnostics
  
plot(mod1d2,type=c("p","smooth"))

plot(mod1d2,sqrt(abs(resid(.)))~fitted(.),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid))))

plot(mod1d2,resid(.,type="pearson")~SetInd, type=c("p","smooth"))

qqnorm(resid(mod1d2))
qqline(resid(mod1d2))

hist(resid(mod1d2),100)

###########################################################################################