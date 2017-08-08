#################################################################################
#
# Analysis using linear mixed effects model (kuppu)
#
#################################################################################

# Created by Paul Thompson, 02-05-2017

library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(lme4)
library(yarrr)

#RT_data<-read.csv("C:\\Users\\ksengottuvel\\Dropbox\\KUPPU and DOROTHY\\SL and WM paper\\DRAFTS\\Pre-reg drafts\\first submission\\SL, Pre-reg, OSF Files\\Pilot_1_SST.csv")

#windows(record=T)

namelist=c('Pilot_1_SST','Pilot_2_SST','Pilot_3_SST','Pilot_4_SST','Pilot_5_SST','Pilot_6_SST','Pilot_7_SST','Pilot_8_SST','Pilot_9_SST','Pilot_10_SST' )
nsubs=length(namelist)

mm_data_big<-matrix(NA,3000,18)


for (i in 1:nsubs){
  myname=namelist[i]
  mycsv=paste("C:\\Users\\pthompson\\Dropbox\\SL and WM paper\\DRAFTS\\Pre-reg drafts\\first submission\\SL, Pre-reg, OSF Files\\",myname,".csv",sep="")
  mydata= read.csv(mycsv)  # read csv file 
  
  rawdata=mydata
  rawdata$Dep2RT[rawdata$Dep2ACC==0]<-NA
  rawdata$CondRT[rawdata$CondACC==0]<-NA
  rawdata$Dep1RT[rawdata$Dep1ACC==0]<-NA
  rawdata$Dep2RT[rawdata$Dep2RT>2000]=NA
  rawdata$CondRT[rawdata$CondRT>2000]=NA
  rawdata$Dep1RT[rawdata$Dep1RT>2000]=NA
  rawdata$Dep2RT[rawdata$Dep2RT<100]=NA
  rawdata$CondRT[rawdata$CondRT<100]=NA
  rawdata$Dep1RT[rawdata$Dep1RT<100]=NA
  #get rid of columns not necessary  
  
  #id =rawdata$ID[5]; ID=as.character(id);sex=rawdata$Gender[5];Sex=as.character(sex);Age=rawdata$Age[5];
  #group=rawdata$Group[5];Group=as.character(group);

  alldata=rawdata[,c(1,2,3,4,8,9,10,12,18:19,20,21,28,29,30)]; #picks only that are needed-easy to look at
  ###################since the pilot task, had Types reshuffled, the following section renames them appropriately. This section is now fixed in task,
  #in the original analysis the following section will be removed
  
  alldata$Oldtype<-alldata$Type #save original version of Type
  
  
  alldata$Type[which(alldata$Dep1TgtCode==1001)]<-1
  alldata$Type[which(alldata$Dep1TgtCode==1003)]<-2
  alldata$Type[which(alldata$Dep2TgtCode==1006)]<-3
  alldata$Type[which(alldata$Dep2TgtCode==1007)]<-3
  alldata$Type[which(alldata$Dep2TgtCode==1009)]<-4
  alldata$Type[which(alldata$Dep2TgtCode==1010)]<-4
  alldata$Type[which(alldata$Dep1TgtCode==1011)]<-5
  
  
  ##################################################################
  library(car)
  
  red_data<-alldata
  
  red_data$Dis_TP<-car::recode(red_data$Type,"1='nd_DT';2='ad_DT';3='nd_PR';4='ad_PR'")
  red_data$Dist<-car::recode(red_data$Type,"1='nd';2='ad';3='nd';4='ad'")
  red_data$TP<-car::recode(red_data$Type,"1='DT';2='DT';3='PR';4='PR'")
  rename(red_data, c("Dep2RT" = "RT"))
  
  if(i==1){mm_data_big<-red_data}
  else{
    mm_data_big<-rbind(mm_data_big,red_data)
  }
  
  
}
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################


##################statistical analysis #################

#############################use the mm_data ####################
mm_data_big$Type<-as.numeric(mm_data_big$Type)
mm_data_big$Block<-as.numeric(mm_data_big$Block)

mm_data_big$ID<-as.factor(mm_data_big$ID)
mm_data_big$Dis_TP<-as.factor(mm_data_big$Dis_TP)
mm_data_big$Dis_TP<-factor(mm_data_big$Dis_TP,levels=c("ad_DT", "nd_DT", "ad_PR", "nd_PR"))

mm_data_big$Dist<-as.factor(mm_data_big$Dist)
mm_data_big$TP<-as.factor(mm_data_big$TP)

mm_data_big$TP<-factor(mm_data_big$TP, levels=c("5","PR","DT"))

mm_data_big$SetInd<-as.factor(mm_data_big$SetInd)


#library(ggplot2)

ggplot(mm_data_big, aes(x=SetInd, y=Dep2RT)) + geom_boxplot() + geom_smooth(aes(group = ID,colour = ID), method = "loess", se = FALSE)+ facet_wrap(~Type)

ggplot(mm_data_big, aes(x=SetInd, y=Dep2RT)) + geom_boxplot() + geom_smooth(aes(group = ID,colour = ID), method = "loess", se = FALSE)+ facet_wrap(~TP)





#boxplots

pdf("pirate_plots.pdf")
pirateplot(RT~id,data=mm_data,xlab="Participant",ylab="RT")
pirateplot(RT~Block,data=mm_data,xlab="Block",ylab="RT")
pirateplot(RT~Dis_TP,data=mm_data,xlab="Dis_TP",ylab="RT")
pirateplot(RT~Dist,data=mm_data,xlab="Dist",ylab="RT")
pirateplot(RT~TP,data=mm_data,xlab="TP",ylab="RT")

pirateplot(RT~TP+Block,data=mm_data,ylab="RT")
pirateplot(RT~Dist+Block,data=mm_data,ylab="RT")
dev.off()

#Mixed effects model for block, Dist and TP. Random intercepts for participants and random slopes for Block. Fixed effects for Dist and TP.
mixed_A<-lmer(RT~ 1 + Block + Dist + TP + (Block-1|ID) , data=mm_data) 


#Likelihood ratio tests

mixed0<-lmer(RT~ 1 + (1|ID) , data=mm_data, REML=FALSE)

mixed1<-lmer(RT~ 1 + Block + (Block-1|ID) , data=mm_data, REML=FALSE) 

mixed2<-lmer(RT~ 1 + Block + Dist + (Block-1|ID), data=mm_data, REML=FALSE)

mixed3<-lmer(RT~ 1 + Block + TP + (Block-1|ID), data=mm_data, REML=FALSE)

mixed4A<-lmer(RT~ 1 + Block + TP + TP:Block + (Block-1|ID), data=mm_data, REML=FALSE)

mixed5a<-lmer(RT~ 1 + Block + TP + Block:TP + (TP + Block-1|ID), data=mm_data, REML=FALSE)

anova(mixed0,mixed1) #Does the random slopes for Block make a difference?
anova(mixed1,mixed2) #Is there a 'Dist' effect? No
anova(mixed1,mixed3) #Is there a 'TP' effect? Yes, as predicted looking at the pirate plots.
anova(mixed4A,mixed5A)
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################



