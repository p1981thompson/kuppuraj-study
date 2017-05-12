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

mm_data<-matrix(NA,20,7)


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
  
  rawdata$Dep2RT[rawdata$Dep2Acc==0]=NA
  
  alldata=rawdata[,c(1,2,3,4,8,9,10,12,19,20,21,28,29,30)]; #picks only that are needed-easy to look at
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
  
  red_data<-alldata#[!alldata$Type==5,]



  meds1<-aggregate(x = red_data$Dep2RT, by = list(red_data$Type,red_data$Block), FUN = median, na.rm=T)
  
  names(meds1)<-c("Type","Block","RT")
  meds1$ID<-rep(myname,length(meds1$Type))
  
  meds1$Dis_TP<-car::recode(meds1$Type,"1='nonD';2='adjD';3='nonP';4='adjP';5='rand'")
  #meds1$Dist<-car::recode(meds1$Type,"1='nd';2='ad';3='nd';4='ad'")
  #meds1$TP<-car::recode(meds1$Type,"1='DT';2='DT';3='PR';4='PR'")
  
  if(i==1){mm_data<-meds1}
  else{
    mm_data<-rbind(mm_data,meds1)
  }
  
  
}
  #########################################################################################################################
  #########################################################################################################################
  #########################################################################################################################
  

##################statistical analysis #################

#############################use the mm_data ####################
mm_data$Type<-as.numeric(mm_data$Type)
mm_data$Block<-as.numeric(scale(mm_data$Block))

mm_data$ID<-as.factor(mm_data$ID)
mm_data$Dis_TP<-as.factor(mm_data$Dis_TP)
mm_data$Dis_TP<-factor(mm_data$Dis_TP,levels=c("rand","adjD", "nonD", "adjP", "nonP"))

mm_data$RT<-scale(mm_data$RT)
#mm_data$TP<-as.factor(mm_data$TP)



#boxplots

# pdf("pirate_plots.pdf")
# pirateplot(RT~id,data=mm_data,xlab="Participant",ylab="RT")
# pirateplot(RT~Block,data=mm_data,xlab="Block",ylab="RT")
# pirateplot(RT~Dis_TP,data=mm_data,xlab="Dis_TP",ylab="RT")
# pirateplot(RT~Dist,data=mm_data,xlab="Dist",ylab="RT")
# pirateplot(RT~TP,data=mm_data,xlab="TP",ylab="RT")
# 
# pirateplot(RT~TP+Block,data=mm_data,ylab="RT")
# pirateplot(RT~Dist+Block,data=mm_data,ylab="RT")
# dev.off()
# 
# #Mixed effects model for block, Dist and TP. Random intercepts for participants and random slopes for Block. Fixed effects for Dist and TP.
# mixed_A<-lmer(RT~ 1 + Block + Dist + TP + (Block-1|ID) , data=mm_data) 
# 
# 
# #Likelihood ratio tests
# 
# mixed0<-lmer(RT~ 1 + (1|ID) , data=mm_data, REML=FALSE)
# 
# mixed1<-lmer(RT~ 1 + Block + (Block-1|ID) , data=mm_data, REML=FALSE) 
# 
# mixed2<-lmer(RT~ 1 + Block + Dist + (Block-1|ID), data=mm_data, REML=FALSE)
# 
# mixed3<-lmer(RT~ 1 + Block + TP + (Block-1|ID), data=mm_data, REML=FALSE)
# 
# mixed4A<-lmer(RT~ 1 + Block + TP + TP:Block + (Block-1|ID), data=mm_data, REML=FALSE)
# 
# mixed5a<-lmer(RT~ 1 + Block + TP + Block:TP + (TP + Block-1|ID), data=mm_data, REML=FALSE)
# 
# anova(mixed0,mixed1) #Does the random slopes for Block make a difference?
# anova(mixed1,mixed2) #Is there a 'Dist' effect? No
# anova(mixed1,mixed3) #Is there a 'TP' effect? Yes, as predicted looking at the pirate plots.
# anova(mixed4A,mixed5A)
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################



