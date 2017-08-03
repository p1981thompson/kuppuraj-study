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



main.data$ID<-as.factor(main.data$ID) 
  #28/07/2017
  
main.data.t1<-subset(main.data,SetInd<=25)
main.data.t2<-subset(main.data,SetInd>25&SetInd<=32)
main.data.t3<-subset(main.data,SetInd>32)
  
  
  
  mod1 <- lmer(TargetRT ~ Type + SetInd + Type*SetInd + (SetInd | ID), data = main.data.t1, REML = FALSE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  mod2 <- lmer(TargetRT ~ Type + SetInd + Type*SetInd + (SetInd | ID), data = main.data.t2, REML = FALSE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  mod3 <- lmer(TargetRT ~ Type + SetInd + Type*SetInd + (SetInd | ID), data = main.data.t3, REML = FALSE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
 
    # ggplot(main.data, aes(x = SetInd, y = TargetRT,group=ID)) + 
    # geom_point(shape = 21,alpha=0.1) + 
    # geom_vline(aes(xintercept = 25), color = 'grey', size = 1, linetype = 'dashed') + 
    # geom_vline(aes(xintercept = 33), color = 'grey', size = 1, linetype = 'dashed') +
    # geom_smooth(data=main.data.t1,aes(y=TargetRT,x=SetInd,color=ID),method="lm",se=F)+
    # geom_smooth(data=main.data.t2,aes(y=TargetRT,x=SetInd,color=ID),method="lm",se=F)+
    # geom_smooth(data=main.data.t3,aes(y=TargetRT,x=SetInd,color=ID),method="lm",se=F)+theme_bw()+facet_grid(~ Condition_type)
    # 
  
  
  # ggplot(main.data, aes(x = SetInd, y = TargetRT)) + 
  # geom_point(shape = 21,alpha=0.5) + 
  # geom_vline(aes(xintercept = 25), color = 'grey', size = 1, linetype = 'dashed') + 
  # geom_vline(aes(xintercept = 33), color = 'grey', size = 1, linetype = 'dashed') +
  # geom_smooth(data=main.data.t1,aes(y=predict(mod1)),method='lm', se=T)+
  # geom_smooth(data=main.data.t2,aes(y=predict(mod2)),method='lm', se=T)+
  # geom_smooth(data=main.data.t3,aes(y=predict(mod3)),method='lm', se=T)+ylim(0,1500)+theme_bw()+facet_grid(ID~Type)
  # 
  
  newdat1<-expand.grid(Type=unique(main.data$Type),SetInd=1:24,ID=unique(main.data$ID))
  newdat2<-expand.grid(Type=unique(main.data$Type),SetInd=25:32,ID=unique(main.data$ID))
  newdat3<-expand.grid(Type=unique(main.data$Type),SetInd=33:40,ID=unique(main.data$ID))
  
  library(RColorBrewer)
  
  ggplot(main.data, aes(x = SetInd, y = TargetRT,color=Type)) + 
  geom_point(alpha=0.35) + 
  geom_vline(aes(xintercept = 25), color = 'grey', size = 1, linetype = 'dashed') + 
  geom_vline(aes(xintercept = 33), color = 'grey', size = 1, linetype = 'dashed') +
  geom_line(data=newdat1,aes(y=predict(mod1,newdata=newdat1)),size = .75)+
  geom_line(data=newdat2,aes(y=predict(mod2,newdata=newdat2)),size = .75)+
  geom_line(data=newdat3,aes(y=predict(mod3,newdata=newdat3)),size = .75)+theme_bw()+facet_grid(~ID)+ scale_fill_brewer(palette="Set1")+theme(legend.position = "top",strip.text=element_text(size=12),axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))
  


##################################################################

    ggplot(main.data, aes(x = SetInd, y = TargetRT)) + 
      geom_point(shape = 21,alpha=0.5) + 
      geom_vline(aes(xintercept = 25), color = 'grey', size = 1, linetype = 'dashed') + 
      geom_vline(aes(xintercept = 32), color = 'grey', size = 1, linetype = 'dashed') +
      geom_line()+
     theme_bw()+facet_grid(ID~Type)+ylim(0,1500)
    
    
  ggplot(main.data, aes(x = SetInd, y = TargetRT,color=ID)) + 
    geom_point(shape = 21,alpha=0.5) + 
    geom_vline(aes(xintercept = 25), color = 'grey', size = 1, linetype = 'dashed') + 
    geom_vline(aes(xintercept = 32), color = 'grey', size = 1, linetype = 'dashed') +
    geom_line()+
    theme_bw()+facet_grid(~Type)
    
 #########################
    
pdf("c:/Users/pthompson/Desktop/pirate_plots.pdf")
  #do it for all conditions 
  all_cond=pirateplot(formula = TargetRT ~ SetInd + Type,
                      data = main.data,
                      xlab = "Type",
                      ylab = "RT",
                      main = "all_participants/all_conditions")
  
  abline(v=25,col="red",lwd=2)
  abline(v=32,col="red",lwd=2)
  ###
  main.data2<-main.data
  main.data2$SetInd<-as.factor(main.data2$SetInd)
  
  ggplot(main.data2,aes(y=TargetRT,x=SetInd))+geom_boxplot()+facet_grid(~Type)+
  geom_vline(aes(xintercept = 25), color = 'red', size = 1, linetype = 'dashed') + 
    geom_vline(aes(xintercept = 32), color = 'red', size = 1, linetype = 'dashed') +theme_bw()
  
###
  
  #do it by condition 
  con_2_data=subset(main.data, main.data$Type=='Adj_P')
  
  cond_2_AP=pirateplot(formula = TargetRT ~ SetInd+Type,
                       data = con_2_data,
                       xlab = "Type",
                       ylab = "RT",
                       main = "all_participants/AP")
  
  abline(v=25,col="red",lwd=2)
  abline(v=32,col="red",lwd=2)
  
  
  con_1_data=subset(main.data, main.data$Type=='Adj_D')
  
  cond_1_AD=pirateplot(formula = TargetRT ~ SetInd+Type,
                       data = con_1_data,
                       xlab = "Type",
                       ylab = "RT",
                       main = "all_participants/AD")
  
  abline(v=25,col="red",lwd=2)
  abline(v=32,col="red",lwd=2)
  
  
  con_3_data=subset(main.data, main.data$Type=='Non_D')
  
  cond_3_ND=pirateplot(formula = TargetRT ~ SetInd+Type,
                       data = con_3_data,
                       xlab = "Type",
                       ylab = "RT",
                       main = "all_participants/ND")
  
  abline(v=25,col="red",lwd=2)
  abline(v=32,col="red",lwd=2)
  
  
  con_4_data=subset(main.data, main.data$Type=='Non_P')
  
  cond_4_NP=pirateplot(formula = TargetRT ~ SetInd+Type,
                       data = con_4_data,
                       xlab = "Type",
                       ylab = "RT",
                       main = "all_participants/NP")
  
  abline(v=25,col="red",lwd=2)
  abline(v=32,col="red",lwd=2)
  
  
  con_5_data=subset(main.data, main.data$Type=='rand')
  
  cond_5_RR=pirateplot(formula = TargetRT ~ SetInd+Type,
                       data = con_5_data,
                       xlab = "Type",
                       ylab = "RT",
                       main = "all_participants/R")
  
  abline(v=25,col="red",lwd=2)
  abline(v=32,col="red",lwd=2)
  
  dev.off()
  
  
  
  #############################################################
  
  install.packages("forecast")
  library(forecast)
  
  
  windows(record=T)
  pdf("c:/Users/pthompson/Desktop/decompose.pdf")
  my_ids<-as.numeric(names(table(main.data$ID)))
  type_name<-c(names(table(main.data$Type)))
  
  for(i in 1:6){
    for(j in 1:5)
    {
  # trend1<-ma(as.numeric(main.data$TargetRT[main.data$ID==2 & main.data$Type=="Adj_D"]),order = 4, centre = T)
  # plot(as.ts(as.numeric(main.data$TargetRT[main.data$ID==2 & main.data$Type=="Adj_D"])))
  # lines(trend1)
  # 
  # detrend1 = as.ts(as.numeric(main.data$TargetRT[main.data$ID==2 & main.data$Type=="Adj_D"])) - trend1
  # plot(as.ts(detrend1))
  # 
  
  ts1 = ts(as.numeric(main.data$TargetRT[main.data$ID==my_ids[i] & main.data$Type==type_name[j]]), frequency = 2)
  decompose1 = decompose(ts1, "additive")

  plot(decompose1)
    }
  }
  dev.off()
  
  ggplot(main.data, aes(x = SetInd, y = TargetRT)) + 
    geom_point(shape = 21,alpha=0.5) + 
    geom_vline(aes(xintercept = 25), color = 'grey', size = 1, linetype = 'dashed') + 
    geom_vline(aes(xintercept = 32), color = 'grey', size = 1, linetype = 'dashed') +
    geom_line()+
    theme_bw()+facet_grid(ID~Type)+ylim(0,1500)
  
  